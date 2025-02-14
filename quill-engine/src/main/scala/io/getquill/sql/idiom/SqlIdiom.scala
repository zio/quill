package io.getquill.context.sql.idiom

import com.github.takayahilton.sqlformatter._
import io.getquill.{IdiomContext, NamingStrategy}
import io.getquill.ast.BooleanOperator._
import io.getquill.ast.Renameable.Fixed
import io.getquill.ast.Visibility.Hidden
import io.getquill.ast._
import io.getquill.context.sql._
import io.getquill.context.sql.idiom.SqlIdiom.InsertUpdateStmt
import io.getquill.context.sql.idiom.SqlIdiom.ActionTableAliasBehavior
import io.getquill.context.sql.norm._
import io.getquill.context.{ExecutionType, OutputClauseSupported, ReturningCapability, ReturningClauseSupported}
import io.getquill.idiom.StatementInterpolator._
import io.getquill.idiom._
import io.getquill.norm.ConcatBehavior.AnsiConcat
import io.getquill.norm.EqualityBehavior.AnsiEquality
import io.getquill.norm.{ConcatBehavior, EqualityBehavior, ExpandReturning, NormalizeCaching, ProductAggregationToken}
import io.getquill.quat.Quat
import io.getquill.sql.norm.{HideTopLevelFilterAlias, NormalizeFilteredActionAliases, RemoveExtraAlias, RemoveUnusedSelects}
import io.getquill.util.{Interleave, Interpolator, Messages, TraceConfig}
import io.getquill.util.Messages.{TraceType, fail, trace}

trait SqlIdiom extends Idiom {

  def useActionTableAliasAs: ActionTableAliasBehavior = ActionTableAliasBehavior.UseAs

  def makeCache() =
    if (Messages.cacheNormalization)
      SqlNormalizeCaches.unlimitedLocal()
    else
      SqlNormalizeCaches.NoCache

  override def prepareForProbing(string: String): String

  protected def concatBehavior: ConcatBehavior = AnsiConcat

  protected def equalityBehavior: EqualityBehavior               = AnsiEquality
  protected def productAggregationToken: ProductAggregationToken = ProductAggregationToken.Star

  protected def actionAlias: Option[Ident] = None

  override def format(queryString: String): String = SqlFormatter.format(queryString)

  def normalizeAst(
    ast: Ast,
    concatBehavior: ConcatBehavior,
    equalityBehavior: EqualityBehavior,
    idiomContext: IdiomContext
  ) =
    SqlNormalize(ast, idiomContext.config, makeCache(), concatBehavior, equalityBehavior)

  def querifyAst(ast: Ast, traceConfig: TraceConfig) = new SqlQueryApply(traceConfig)(ast)

  // See HideTopLevelFilterAlias for more detail on how this works
  def querifyAction(ast: Action, batchAlias: Option[String]) = {
    val norm1 = new NormalizeFilteredActionAliases(batchAlias)(ast)
    val norm2 = io.getquill.sql.norm.HideInnerProperties(norm1)
    useActionTableAliasAs match {
      case ActionTableAliasBehavior.Hide => HideTopLevelFilterAlias(norm2)
      case _                             => norm2
    }
  }

  private def doTranslate(
    ast: Ast,
    cached: Boolean,
    topLevelQuat: Quat,
    executionType: ExecutionType,
    idiomContext: IdiomContext
  )(implicit naming: NamingStrategy): (Ast, Statement, ExecutionType) = {

    val normalizedAst =
      if (cached) {
        NormalizeCaching((a: Ast) => normalizeAst(a, concatBehavior, equalityBehavior, idiomContext))(ast)
      } else {
        normalizeAst(ast, concatBehavior, equalityBehavior, idiomContext)
      }

    implicit val transpileContextImplicit: IdiomContext = idiomContext
    implicit val tokenizer: Tokenizer[Ast]              = defaultTokenizer
    val interp                                          = new Interpolator(TraceType.SqlNormalizations, idiomContext.traceConfig, 1)
    import interp._

    val token =
      normalizedAst match {
        case q: Query =>
          val sql = querifyAst(q, idiomContext.traceConfig)
          trace"SQL: ${sql}".andLog()
          VerifySqlQuery(sql).map(fail)
          val expanded = ExpandNestedQueries(sql, topLevelQuat)
          trace"Expanded SQL: ${expanded}".andLog()
          val refined = if (Messages.pruneColumns) RemoveUnusedSelects(expanded) else expanded
          trace"Filtered SQL (only used selects): ${refined}".andLog()
          val cleaned = if (!Messages.alwaysAlias) RemoveExtraAlias(naming)(refined, topLevelQuat) else refined
          trace"Cleaned SQL: ${cleaned}".andLog()
          val tokenized = cleaned.token
          trace"Tokenized SQL: ${cleaned}".andLog()
          tokenized
        case a: Action =>
          // Mostly we don't use the alias in SQL set-queries but if we do, make sure they are right
          val sql = querifyAction(a, idiomContext.queryType.batchAlias)
          trace"Action SQL: ${sql}".andLog()
          // Run the tokenization, make sure that we're running tokenization from the top-level (i.e. from the Ast-tokenizer, don't go directly to the action tokenizer)
          (sql: Ast).token
        case other =>
          other.token
      }

    (normalizedAst, stmt"$token", executionType)
  }

  override def translate(ast: Ast, topLevelQuat: Quat, executionType: ExecutionType, idiomContext: IdiomContext)(
    implicit naming: NamingStrategy
  ): (Ast, Statement, ExecutionType) =
    doTranslate(ast, false, topLevelQuat, executionType, idiomContext)

  override def translateCached(ast: Ast, topLevelQuat: Quat, executionType: ExecutionType, idiomContext: IdiomContext)(
    implicit naming: NamingStrategy
  ): (Ast, Statement, ExecutionType) =
    doTranslate(ast, true, topLevelQuat, executionType, idiomContext)

  def defaultTokenizer(implicit naming: NamingStrategy, idiomContext: IdiomContext): Tokenizer[Ast] =
    new Tokenizer[Ast] {
      private val stableTokenizer = astTokenizer(this, naming, idiomContext)

      def token(v: Ast) = stableTokenizer.token(v)
    }

  def astTokenizer(implicit
    astTokenizer: Tokenizer[Ast],
    strategy: NamingStrategy,
    idiomContext: IdiomContext
  ): Tokenizer[Ast] =
    Tokenizer[Ast] {
      case a: Query =>
        // This case typically happens when you have a select inside of an insert
        // infix or a set operation (e.g. query[Person].exists).
        // have a look at the SqlDslSpec `forUpdate` and `insert with subselects` tests
        // for more details.
        // Right now we are not removing extra select clauses here (via RemoveUnusedSelects) since I am not sure what
        // kind of impact that could have on selects. Can try to do that in the future.
        if (Messages.querySubexpand) {
          val nestedExpanded = ExpandNestedQueries(new SqlQueryApply(idiomContext.traceConfig)(a))
          RemoveExtraAlias(strategy)(nestedExpanded).token
        } else
          new SqlQueryApply(idiomContext.traceConfig)(a).token

      case a: Operation       => a.token
      case a: Infix           => a.token
      case a: Action          => a.token
      case a: Ident           => a.token
      case a: ExternalIdent   => a.token
      case a: Property        => a.token
      case a: Value           => a.token
      case a: If              => a.token
      case a: External        => a.token
      case a: Assignment      => a.token
      case a: AssignmentDual  => a.token
      case a: OptionOperation => a.token
      case a @ (
            _: Function | _: FunctionApply | _: Dynamic | _: OptionOperation | _: Block | _: Val | _: Ordering |
            _: QuotedReference | _: IterableOperation | _: OnConflict.Excluded | _: OnConflict.Existing
          ) =>
        fail(s"Malformed or unsupported construct: $a.")
    }

  implicit def ifTokenizer(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy): Tokenizer[If] =
    Tokenizer[If] { case ast: If =>
      def flatten(ast: Ast): (List[(Ast, Ast)], Ast) =
        ast match {
          case If(cond, a, b) =>
            val (l, e) = flatten(b)
            ((cond, a) +: l, e)
          case other =>
            (List.empty, other)
        }

      val (l, e) = flatten(ast)
      val conditions =
        for ((cond, body) <- l) yield {
          stmt"WHEN ${cond.token} THEN ${body.token}"
        }
      stmt"CASE ${conditions.mkStmt(" ")} ELSE ${e.token} END"
    }

  def concatFunction: String

  protected def tokenizeGroupBy(values: Ast)(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy): Token =
    values.token

  protected class FlattenSqlQueryTokenizerHelper(q: FlattenSqlQuery)(implicit
    astTokenizer: Tokenizer[Ast],
    strategy: NamingStrategy,
    idiomContext: IdiomContext
  ) {

    import q._

    def selectTokenizer: Token =
      select match {
        case Nil => stmt"*"
        case _   => select.token
      }

    def distinctTokenizer: Statement = (
      distinct match {
        case DistinctKind.Distinct          => stmt"DISTINCT "
        case DistinctKind.DistinctOn(props) => stmt"DISTINCT ON (${props.token}) "
        case DistinctKind.None              => emptyStatement
      }
    )

    def withDistinct = stmt"$distinctTokenizer${selectTokenizer}"

    def withFrom: Statement =
      from match {
        case Nil => withDistinct
        case head :: tail =>
          val t = tail.foldLeft(stmt"${head.token}") {
            case (a, b: FlatJoinContext) =>
              stmt"$a ${(b: FromContext).token}"
            case (a, b) =>
              stmt"$a, ${b.token}"
          }

          stmt"$withDistinct FROM $t"
      }

    def withWhere: Statement =
      where match {
        case None        => withFrom
        case Some(where) => stmt"$withFrom WHERE ${where.token}"
      }

    def withGroupBy: Statement =
      groupBy match {
        case None          => withWhere
        case Some(groupBy) => stmt"$withWhere GROUP BY ${tokenizeGroupBy(groupBy)}"
      }

    def withOrderBy: Statement =
      orderBy match {
        case Nil     => withGroupBy
        case orderBy => stmt"$withGroupBy ${tokenOrderBy(orderBy)}"
      }

    def withLimitOffset: Token = limitOffsetToken(withOrderBy).token((limit, offset))

    def apply: Statement = stmt"SELECT $withLimitOffset"
  }

  implicit def sqlQueryTokenizer(implicit
    astTokenizer: Tokenizer[Ast],
    strategy: NamingStrategy,
    idiomContext: IdiomContext
  ): Tokenizer[SqlQuery] = Tokenizer[SqlQuery] {
    case q: TopInfixQuery =>
      q.ast.token
    case q: FlattenSqlQuery =>
      new FlattenSqlQueryTokenizerHelper(q).apply
    case SetOperationSqlQuery(a, op, b) =>
      stmt"(${a.token}) ${op.token} (${b.token})"
    case UnaryOperationSqlQuery(op, q) =>
      stmt"SELECT ${op.token} (${q.token})"
  }

  protected def tokenizeColumn(strategy: NamingStrategy, column: String, renameable: Renameable) =
    renameable match {
      case Fixed => tokenizeFixedColumn(strategy, column)
      case _     => strategy.column(column)
    }

  protected def tokenizeTable(strategy: NamingStrategy, table: String, renameable: Renameable) =
    renameable match {
      case Fixed => table
      case _     => strategy.table(table)
    }

  // By default do not change alias of an "AS column" based on naming strategy because the corresponding
  // things using it would also need to change.
  protected def tokenizeColumnAlias(strategy: NamingStrategy, column: String): String =
    column

  protected def tokenizeFixedColumn(strategy: NamingStrategy, column: String): String =
    column

  protected def tokenizeTableAlias(strategy: NamingStrategy, table: String): String =
    table

  protected def tokenizeIdentName(strategy: NamingStrategy, name: String): String =
    name

  implicit def selectValueTokenizer(implicit
    astTokenizer: Tokenizer[Ast],
    strategy: NamingStrategy,
    idiomContext: IdiomContext
  ): Tokenizer[SelectValue] = {

    def tokenizer(implicit astTokenizer: Tokenizer[Ast]) =
      Tokenizer[SelectValue] {

        // ExpandNestedQuery should elaborate all Idents with Product quats so the only ones left are value quats
        // treat them as a id.* because in most SQL dialects identifiers cannot be spliced in by themselves
        case SelectValue(Ident("?", Quat.Value), _, _)  => "?".token
        case SelectValue(Ident(name, Quat.Value), _, _) => stmt"${strategy.default(name).token}.*"

        // Typically these next two will be for Ast Property
        case SelectValue(ast, Some(alias), false) => {
          stmt"${ast.token} AS ${tokenizeColumnAlias(strategy, alias).token}"
        }
        case SelectValue(ast, Some(alias), true) =>
          stmt"${concatFunction.token}(${ast.token}) AS ${tokenizeColumnAlias(strategy, alias).token}"

        // For situations where this is no alias etc...
        case selectValue =>
          val value =
            selectValue match {
              case SelectValue(Ident("?", _), _, _)  => "?".token
              case SelectValue(Ident(name, _), _, _) => stmt"${strategy.default(name).token}.*"
              case SelectValue(ast, _, _)            => ast.token
            }
          selectValue.concat match {
            case true  => stmt"${concatFunction.token}(${value.token})"
            case false => value
          }
      }

    val customAstTokenizer =
      Tokenizer.withFallback[Ast](SqlIdiom.this.astTokenizer(_, strategy, idiomContext)) {

        case Aggregation(op, Ident(id, _: Quat.Product)) => stmt"${op.token}(${makeProductAggregationToken(id)})"
        // Not too many cases of this. Can happen if doing a leaf-level infix inside of a select clause. For example in postgres:
        // `sql"unnest(array['foo','bar'])".as[Query[Int]].groupBy(p => p).map(ap => ap._2.max)` which should yield:
        // SELECT MAX(inf) FROM (unnest(array['foo','bar'])) AS inf GROUP BY inf
        case Aggregation(op, Ident(id, _))   => stmt"${op.token}(${id.token})"
        case Aggregation(op, Tuple(_))       => stmt"${op.token}(*)"
        case Aggregation(op, Distinct(ast))  => stmt"${op.token}(DISTINCT ${ast.token})"
        case ast @ Aggregation(op, _: Query) => scopedTokenizer(ast)
        case Aggregation(op, ast)            => stmt"${op.token}(${ast.token})"
      }

    tokenizer(customAstTokenizer)
  }

  private[getquill] def makeProductAggregationToken(id: String) =
    productAggregationToken match {
      case ProductAggregationToken.Star            => stmt"*"
      case ProductAggregationToken.VariableDotStar => stmt"${id.token}.*"
    }

  implicit def operationTokenizer(implicit
    astTokenizer: Tokenizer[Ast],
    strategy: NamingStrategy
  ): Tokenizer[Operation] = Tokenizer[Operation] {
    case UnaryOperation(op, ast)                               => stmt"${op.token} (${ast.token})"
    case BinaryOperation(a, EqualityOperator.`_==`, NullValue) => stmt"${scopedTokenizer(a)} IS NULL"
    case BinaryOperation(NullValue, EqualityOperator.`_==`, b) => stmt"${scopedTokenizer(b)} IS NULL"
    case BinaryOperation(a, EqualityOperator.`_!=`, NullValue) => stmt"${scopedTokenizer(a)} IS NOT NULL"
    case BinaryOperation(NullValue, EqualityOperator.`_!=`, b) => stmt"${scopedTokenizer(b)} IS NOT NULL"
    case BinaryOperation(a, StringOperator.`startsWith`, b) =>
      stmt"${scopedTokenizer(a)} LIKE (${(BinaryOperation(b, StringOperator.`+`, Constant.auto("%")): Ast).token})"
    case BinaryOperation(a, op @ StringOperator.`split`, b) =>
      stmt"${op.token}(${scopedTokenizer(a)}, ${scopedTokenizer(b)})"
    case BinaryOperation(a, op @ SetOperator.`contains`, b) => SetContainsToken(scopedTokenizer(b), op.token, a.token)
    case BinaryOperation(a, op @ `&&`, b) =>
      (a, b) match {
        case (BinaryOperation(_, `||`, _), BinaryOperation(_, `||`, _)) =>
          stmt"${scopedTokenizer(a)} ${op.token} ${scopedTokenizer(b)}"
        case (BinaryOperation(_, `||`, _), _) => stmt"${scopedTokenizer(a)} ${op.token} ${b.token}"
        case (_, BinaryOperation(_, `||`, _)) => stmt"${a.token} ${op.token} ${scopedTokenizer(b)}"
        case _                                => stmt"${a.token} ${op.token} ${b.token}"
      }
    case BinaryOperation(a, op @ `||`, b) => stmt"${a.token} ${op.token} ${b.token}"
    case BinaryOperation(a, op, b)        => stmt"${scopedTokenizer(a)} ${op.token} ${scopedTokenizer(b)}"
    case e: FunctionApply                 => fail(s"Can't translate the ast to sql: '$e'")
  }

  implicit def optionOperationTokenizer(implicit
    astTokenizer: Tokenizer[Ast],
    strategy: NamingStrategy
  ): Tokenizer[OptionOperation] = Tokenizer[OptionOperation] {
    case OptionIsEmpty(ast)   => stmt"${ast.token} IS NULL"
    case OptionNonEmpty(ast)  => stmt"${ast.token} IS NOT NULL"
    case OptionIsDefined(ast) => stmt"${ast.token} IS NOT NULL"
    case OptionNone(_)        => stmt"null"
    case other                => fail(s"Malformed or unsupported construct: $other.")
  }

  implicit val setOperationTokenizer: Tokenizer[SetOperation] = Tokenizer[SetOperation] {
    case UnionOperation    => stmt"UNION"
    case UnionAllOperation => stmt"UNION ALL"
  }

  protected def limitOffsetToken(query: Statement)(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy) =
    Tokenizer[(Option[Ast], Option[Ast])] {
      case (None, None)                => query
      case (Some(limit), None)         => stmt"$query LIMIT ${limit.token}"
      case (Some(limit), Some(offset)) => stmt"$query LIMIT ${limit.token} OFFSET ${offset.token}"
      case (None, Some(offset))        => stmt"$query OFFSET ${offset.token}"
    }

  protected def tokenOrderBy(
    criteria: List[OrderByCriteria]
  )(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy) =
    stmt"ORDER BY ${criteria.token}"

  implicit def sourceTokenizer(implicit
    astTokenizer: Tokenizer[Ast],
    strategy: NamingStrategy,
    idiomContext: IdiomContext
  ): Tokenizer[FromContext] = Tokenizer[FromContext] {
    case TableContext(name, alias)  => stmt"${name.token} ${tokenizeTableAlias(strategy, alias).token}"
    case QueryContext(query, alias) => stmt"(${query.token})${` AS`} ${tokenizeTableAlias(strategy, alias).token}"
    case InfixContext(infix, alias) =>
      stmt"(${(infix: Ast).token})${` AS`} ${tokenizeTableAlias(strategy, alias).token}"
    case JoinContext(t, a, b, on)  => stmt"${a.token} ${t.token} ${b.token} ON ${on.token}"
    case FlatJoinContext(t, a, on) => stmt"${t.token} ${a.token} ON ${on.token}"
  }

  private def ` AS` =
    useActionTableAliasAs match {
      case ActionTableAliasBehavior.UseAs => stmt" AS"
      case _                              => emptyStatement
    }

  implicit val joinTypeTokenizer: Tokenizer[JoinType] = Tokenizer[JoinType] {
    case InnerJoin => stmt"INNER JOIN"
    case LeftJoin  => stmt"LEFT JOIN"
    case RightJoin => stmt"RIGHT JOIN"
    case FullJoin  => stmt"FULL JOIN"
  }

  implicit def orderByCriteriaTokenizer(implicit
    astTokenizer: Tokenizer[Ast],
    strategy: NamingStrategy
  ): Tokenizer[OrderByCriteria] = Tokenizer[OrderByCriteria] {
    case OrderByCriteria(ast, Asc)            => stmt"${scopedTokenizer(ast)} ASC"
    case OrderByCriteria(ast, Desc)           => stmt"${scopedTokenizer(ast)} DESC"
    case OrderByCriteria(ast, AscNullsFirst)  => stmt"${scopedTokenizer(ast)} ASC NULLS FIRST"
    case OrderByCriteria(ast, DescNullsFirst) => stmt"${scopedTokenizer(ast)} DESC NULLS FIRST"
    case OrderByCriteria(ast, AscNullsLast)   => stmt"${scopedTokenizer(ast)} ASC NULLS LAST"
    case OrderByCriteria(ast, DescNullsLast)  => stmt"${scopedTokenizer(ast)} DESC NULLS LAST"
  }

  implicit val unaryOperatorTokenizer: Tokenizer[UnaryOperator] = Tokenizer[UnaryOperator] {
    case NumericOperator.`-`          => stmt"-"
    case BooleanOperator.`!`          => stmt"NOT"
    case StringOperator.`toUpperCase` => stmt"UPPER"
    case StringOperator.`toLowerCase` => stmt"LOWER"
    case StringOperator.`toLong`      => emptyStatement // cast is implicit
    case StringOperator.`toInt`       => emptyStatement // cast is implicit
    case SetOperator.`isEmpty`        => stmt"NOT EXISTS"
    case SetOperator.`nonEmpty`       => stmt"EXISTS"
  }

  implicit val aggregationOperatorTokenizer: Tokenizer[AggregationOperator] = Tokenizer[AggregationOperator] {
    case AggregationOperator.`min`  => stmt"MIN"
    case AggregationOperator.`max`  => stmt"MAX"
    case AggregationOperator.`avg`  => stmt"AVG"
    case AggregationOperator.`sum`  => stmt"SUM"
    case AggregationOperator.`size` => stmt"COUNT"
  }

  implicit val binaryOperatorTokenizer: Tokenizer[BinaryOperator] = Tokenizer[BinaryOperator] {
    case EqualityOperator.`_==`      => stmt"="
    case EqualityOperator.`_!=`      => stmt"<>"
    case BooleanOperator.`&&`        => stmt"AND"
    case BooleanOperator.`||`        => stmt"OR"
    case StringOperator.`+`          => stmt"||"
    case StringOperator.`startsWith` => fail("bug: this code should be unreachable")
    case StringOperator.`split`      => stmt"SPLIT"
    case NumericOperator.`-`         => stmt"-"
    case NumericOperator.`+`         => stmt"+"
    case NumericOperator.`*`         => stmt"*"
    case NumericOperator.`>`         => stmt">"
    case NumericOperator.`>=`        => stmt">="
    case NumericOperator.`<`         => stmt"<"
    case NumericOperator.`<=`        => stmt"<="
    case NumericOperator.`/`         => stmt"/"
    case NumericOperator.`%`         => stmt"%"
    case SetOperator.`contains`      => stmt"IN"
  }

  implicit def propertyTokenizer(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy): Tokenizer[Property] =
    Tokenizer[Property] {
      case Property.Opinionated(ast, name, renameable, _ /* Top level property cannot be invisible */ ) =>
        // When we have things like Embedded tables, properties inside of one another needs to be un-nested.
        // E.g. in `Property(Property(Ident("realTable"), embeddedTableAlias), realPropertyAlias)` the inner
        // property needs to be unwrapped and the result of this should only be `realTable.realPropertyAlias`
        // as opposed to `realTable.embeddedTableAlias.realPropertyAlias`.

        TokenizeProperty.unnest(ast) match {
          case (ExternalIdent.Opinionated(_: String, _, prefixRenameable), prefix) =>
            stmt"${actionAlias.map(alias => stmt"${scopedTokenizer(alias)}.").getOrElse(emptyStatement)}${TokenizeProperty(name, prefix, strategy, renameable, prefixRenameable)}"

          // When using ExternalIdent such as .returning(eid => eid.idColumn) clauses drop the 'eid' since SQL
          // returning clauses have no alias for the original table. I.e. INSERT [...] RETURNING idColumn there's no
          // alias you can assign to the INSERT [...] clause that can be used as a prefix to 'idColumn'.
          // In this case, `Property(Property(Ident("realTable"), embeddedTableAlias), realPropertyAlias)`
          // should just be `realPropertyAlias` as opposed to `realTable.realPropertyAlias`.
          // The exception to this is when a Query inside of a RETURNING clause is used. In that case, assume
          // that there is an alias for the inserted table (i.e. `INSERT ... as theAlias values ... RETURNING`)
          // and the instances of ExternalIdent use it.
          case (ExternalIdent(_, _), prefix) =>
            stmt"${actionAlias.map(alias => stmt"${scopedTokenizer(alias)}.").getOrElse(emptyStatement)}${TokenizeProperty(name, prefix, strategy, renameable)}"

          // In the rare case that the Ident is invisible, do not show it. See the Ident documentation for more info.
          case (Ident.Opinionated(_, _, Hidden), prefix) =>
            stmt"${TokenizeProperty(name, prefix, strategy, renameable)}"

          // The normal case where `Property(Property(Ident("realTable"), embeddedTableAlias), realPropertyAlias)`
          // becomes `realTable.realPropertyAlias`.
          case (ast, prefix) =>
            stmt"${scopedTokenizer(ast)}.${TokenizeProperty(name, prefix, strategy, renameable)}"
        }
    }

  object TokenizeProperty {
    def unnest(ast: Ast): (Ast, List[String]) =
      ast match {
        case Property.Opinionated(a, _, _, Hidden) =>
          unnest(a) match {
            case (a, nestedName) => (a, nestedName)
          }
        // Append the property name. This includes tuple indexes.
        case Property(a, name) =>
          unnest(a) match {
            case (ast, nestedName) =>
              (ast, nestedName :+ name)
          }
        case e @ ExternalIdent.Opinionated(a, _, Fixed) => (e, List(a))
        case a                                          => (a, Nil)
      }

    def apply(
      name: String,
      prefix: List[String],
      strategy: NamingStrategy,
      renameable: Renameable,
      prefixRenameable: Renameable = Renameable.neutral
    ) =
      prefixRenameable match {
        case Renameable.Fixed =>
          // Typically this happens in a nested query on an multi-level select e.g.
          // SELECT foobar FROM (SELECT foo.bar /*<- this*/ AS foobar ...)
          (tokenizeColumn(strategy, prefix.mkString, prefixRenameable) + "." + tokenizeColumn(
            strategy,
            name,
            renameable
          )).token
        case _ =>
          // Typically this happens on the outer (i.e. top-level) clause of a multi-level select e.g.
          // SELECT foobar /*<- this*/ FROM (SELECT foo.bar AS foobar ...)
          tokenizeColumn(strategy, prefix.mkString + name, renameable).token
      }
  }

  implicit def valueTokenizer(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy): Tokenizer[Value] =
    Tokenizer[Value] {
      case Constant(v: String, _) => stmt"'${v.token}'"
      case Constant((), _)        => stmt"1"
      case Constant(v, _)         => stmt"${v.toString.token}"
      case NullValue              => stmt"null"
      case Tuple(values)          => stmt"${values.token}"
      case CaseClass(_, values)   => stmt"${values.map(_._2).token}"
    }

  implicit def infixTokenizer(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy): Tokenizer[Infix] =
    Tokenizer[Infix] { case Infix(parts, params, _, _, _) =>
      val pt = parts.map(_.token)
      val pr = params.map(_.token)
      Statement(Interleave(pt, pr))
    }

  implicit def identTokenizer(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy): Tokenizer[Ident] =
    Tokenizer[Ident](e => tokenizeIdentName(strategy, e.name).token)

  implicit def externalIdentTokenizer(implicit
    astTokenizer: Tokenizer[Ast],
    strategy: NamingStrategy
  ): Tokenizer[ExternalIdent] =
    Tokenizer[ExternalIdent](e => tokenizeIdentName(strategy, e.name).token)

  implicit def assignmentTokenizer(implicit
    astTokenizer: Tokenizer[Ast],
    strategy: NamingStrategy
  ): Tokenizer[Assignment] = Tokenizer[Assignment] { case a @ Assignment(alias, prop, value) =>
    // Typically we can't use aliases in the SET clause i.e. `UPDATE Person p SET p.name = 'Joe'` doesn't work, it needs to be SET name = 'Joe'`.
    // Strangely, MySQL is the only thing that seems to support this aliasing feature but it is not needed.
    prop match {
      case Property.Opinionated(ast, name, renameable, _) =>
        val columnToken = tokenizeColumn(strategy, name, renameable).token
        stmt"${columnToken} = ${scopedTokenizer(value)}"
      case _ => fail(s"Invalid assignment value of ${a}. Must be a Property object.")
    }
  }

  implicit def assignmentDualTokenizer(implicit
    astTokenizer: Tokenizer[Ast],
    strategy: NamingStrategy
  ): Tokenizer[AssignmentDual] = Tokenizer[AssignmentDual] { case AssignmentDual(alias1, alias2, prop, value) =>
    stmt"${prop.token} = ${scopedTokenizer(value)}"
  }

  implicit def defaultAstTokenizer(implicit
    astTokenizer: Tokenizer[Ast],
    strategy: NamingStrategy,
    idiomContext: IdiomContext
  ): Tokenizer[Action] = {
    val insertEntityTokenizer = Tokenizer[Entity] { case Entity.Opinionated(name, _, _, renameable) =>
      stmt"INTO ${tokenizeTable(strategy, name, renameable).token}"
    }
    actionTokenizer(insertEntityTokenizer)(actionAstTokenizer, strategy, idiomContext)
  }

  protected def actionAstTokenizer(implicit
    astTokenizer: Tokenizer[Ast],
    strategy: NamingStrategy,
    idiomContext: IdiomContext
  ) =
    Tokenizer.withFallback[Ast](SqlIdiom.this.astTokenizer(_, strategy, idiomContext)) {
      case q: Query => astTokenizer.token(q)
      case Property(Property.Opinionated(_, name, renameable, _), "isEmpty") =>
        stmt"${renameable.fixedOr(name)(tokenizeColumn(strategy, name, renameable)).token} IS NULL"
      case Property(Property.Opinionated(_, name, renameable, _), "isDefined") =>
        stmt"${renameable.fixedOr(name)(tokenizeColumn(strategy, name, renameable)).token} IS NOT NULL"
      case Property(Property.Opinionated(_, name, renameable, _), "nonEmpty") =>
        stmt"${renameable.fixedOr(name)(tokenizeColumn(strategy, name, renameable)).token} IS NOT NULL"
      // Used for properties in onConflict etc...
      case Property.Opinionated(_, name, renameable, _) =>
        renameable.fixedOr(name.token)(tokenizeColumn(strategy, name, renameable).token)
    }

  def returnListTokenizer(implicit
    tokenizer: Tokenizer[Ast],
    strategy: NamingStrategy,
    idiomContext: IdiomContext
  ): Tokenizer[List[Ast]] = {
    val customAstTokenizer =
      Tokenizer.withFallback[Ast](SqlIdiom.this.astTokenizer(_, strategy, idiomContext)) { case sq: Query =>
        stmt"(${tokenizer.token(sq)})"
      }

    Tokenizer[List[Ast]] { case list =>
      list.mkStmt(", ")(customAstTokenizer)
    }
  }

  private[getquill] def ` AS [table]`(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy) =
    useActionTableAliasAs match {
      case ActionTableAliasBehavior.UseAs =>
        actionAlias.map(alias => stmt" AS ${alias.token}").getOrElse(emptyStatement)
      case ActionTableAliasBehavior.SkipAs => actionAlias.map(alias => stmt" ${alias.token}").getOrElse(emptyStatement)
      case ActionTableAliasBehavior.Hide   => emptyStatement
    }

  private[getquill] def returningEnabled = !Messages.disableReturning

  protected def actionTokenizer(
    insertEntityTokenizer: Tokenizer[Entity]
  )(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy, idiomContext: IdiomContext): Tokenizer[Action] =
    Tokenizer[Action] {

      case action @ Update(Filter(_: Entity, alias, _), _) =>
        val InsertUpdateStmt(actionToken, whereToken) = SqlIdiom.withActionAlias(this, action, alias)
        stmt"$actionToken WHERE $whereToken"
      case action @ Delete(Filter(_: Entity, alias, _)) =>
        val InsertUpdateStmt(actionToken, whereToken) = SqlIdiom.withActionAlias(this, action, alias)
        stmt"$actionToken WHERE $whereToken"

      case Insert(entity: Entity, assignments) =>
        val (table, columns, values) = insertInfo(insertEntityTokenizer, entity, assignments)
        stmt"INSERT $table${` AS [table]`} (${columns
            .mkStmt(",")}) VALUES ${ValuesClauseToken(stmt"(${values.mkStmt(", ")})")}"

      case Update(table: Entity, assignments) =>
        stmt"UPDATE ${table.token}${` AS [table]`} SET ${assignments.token}"

      case Delete(table: Entity) =>
        stmt"DELETE FROM ${table.token}${` AS [table]`}"

      case r @ ReturningAction(Insert(table: Entity, Nil), alias, prop) =>
        idiomReturningCapability match {
          // If there are queries inside of the returning clause we are forced to alias the inserted table (see #1509). Only do this as
          // a last resort since it is not even supported in all Postgres versions (i.e. only after 9.5)
          case ReturningClauseSupported if (CollectAst.byType[Entity](prop).nonEmpty && returningEnabled) =>
            SqlIdiom.withActionAlias(this, r)
          case ReturningClauseSupported if (returningEnabled) =>
            stmt"INSERT INTO ${table.token} ${defaultAutoGeneratedToken(prop.token)} RETURNING ${tokenizeReturningClause(r)}"
          case OutputClauseSupported if (returningEnabled) =>
            stmt"INSERT INTO ${table.token} OUTPUT ${tokenizeReturningClause(r, Some("INSERTED"))} ${defaultAutoGeneratedToken(prop.token)}"
          case other =>
            stmt"INSERT INTO ${table.token} ${defaultAutoGeneratedToken(prop.token)}"
        }

      case r @ ReturningAction(action, alias, prop) =>
        idiomReturningCapability match {
          // If there are queries inside of the returning clause we are forced to alias the inserted table (see #1509). Only do this as
          // a last resort since it is not even supported in all Postgres versions (i.e. only after 9.5)
          case ReturningClauseSupported if (CollectAst.byType[Entity](prop).nonEmpty && returningEnabled) =>
            SqlIdiom.withActionAlias(this, r)
          case ReturningClauseSupported if (returningEnabled) =>
            stmt"${action.token} RETURNING ${tokenizeReturningClause(r)}"
          case OutputClauseSupported if (returningEnabled) =>
            action match {
              case Insert(entity: Entity, assignments) =>
                val (table, columns, values) = insertInfo(insertEntityTokenizer, entity, assignments)
                stmt"INSERT $table${` AS [table]`} (${columns.mkStmt(",")}) OUTPUT ${returnListTokenizer.token(
                    ExpandReturning(r, Some("INSERTED"))(this, strategy, idiomContext).map(_._1)
                  )} VALUES ${ValuesClauseToken(stmt"(${values.mkStmt(", ")})")}"

              // query[Person].filter(...).update/updateValue(...)
              case action @ Update(Filter(_: Entity, alias, _), _) =>
                val InsertUpdateStmt(actionToken, whereToken) = SqlIdiom.withActionAlias(this, action, alias)
                stmt"$actionToken OUTPUT ${tokenizeReturningClause(r, Some("INSERTED"))} WHERE $whereToken"
              // query[Person].update/updateValue(...)
              case Update(_, _) =>
                stmt"${action.token} OUTPUT ${tokenizeReturningClause(r, Some("INSERTED"))}"

              // query[Person].filter(...).delete/deleteValue(...)
              case action @ Delete(Filter(_: Entity, alias, _)) =>
                val InsertUpdateStmt(actionToken, whereToken) = SqlIdiom.withActionAlias(this, action, alias)
                stmt"$actionToken OUTPUT ${tokenizeReturningClause(r, Some("DELETED"))} WHERE $whereToken"
              // query[Person].delete/deleteValue(...)
              case Delete(_) =>
                stmt"${action.token} OUTPUT ${tokenizeReturningClause(r, Some("DELETED"))}"
              case other =>
                fail(s"Action ast can't be translated to sql: '$other'")
            }
          case _ =>
            stmt"${action.token}"
        }

      case other =>
        fail(s"Action ast can't be translated to sql: '$other'")
    }

  def tokenizeReturningClause(r: ReturningAction, alias: Option[String] = None)(implicit
    astTokenizer: Tokenizer[Ast],
    strategy: NamingStrategy,
    idiomContext: IdiomContext
  ) =
    returnListTokenizer.token(ExpandReturning(r, alias)(this, strategy, idiomContext).map(_._1))

  private def insertInfo(
    insertEntityTokenizer: Tokenizer[Entity],
    entity: Entity,
    assignments: List[Assignment]
  )(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy) = {
    val table             = insertEntityTokenizer.token(entity)
    val (columns, values) = columnsAndValues(assignments)
    (table, columns, values)
  }

  private[getquill] def columnsAndValues(
    assignments: List[Assignment]
  )(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy) = {
    val columns =
      assignments.map(assignment =>
        assignment.property match {
          case Property.Opinionated(_, key, renameable, visibility) => tokenizeColumn(strategy, key, renameable).token
          case _                                                    => fail(s"Invalid assignment value of ${assignment}. Must be a Property object.")
        }
      )
    val values = assignments.map(assignment => scopedTokenizer(assignment.value))
    (columns, values)
  }

  implicit def entityTokenizer(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy): Tokenizer[Entity] =
    Tokenizer[Entity] { case Entity.Opinionated(name, _, _, renameable) =>
      tokenizeTable(strategy, name, renameable).token
    }

  protected def scopedTokenizer(ast: Ast)(implicit tokenizer: Tokenizer[Ast]) =
    ast match {
      case _: Query           => stmt"(${ast.token})"
      case _: BinaryOperation => stmt"(${ast.token})"
      case _: Tuple           => stmt"(${ast.token})"
      case _                  => ast.token
    }
}

object SqlIdiom {
  sealed trait ActionTableAliasBehavior
  object ActionTableAliasBehavior {
    case object UseAs  extends ActionTableAliasBehavior
    case object SkipAs extends ActionTableAliasBehavior
    case object Hide   extends ActionTableAliasBehavior
  }

  private[getquill] def copyIdiom(parent: SqlIdiom, newActionAlias: Option[Ident]) =
    new SqlIdiom {
      override def useActionTableAliasAs: ActionTableAliasBehavior = parent.useActionTableAliasAs

      override protected def actionAlias: Option[Ident] = newActionAlias

      override def prepareForProbing(string: String): String = parent.prepareForProbing(string)

      override def concatFunction: String = parent.concatFunction

      override def liftingPlaceholder(index: Int): String = parent.liftingPlaceholder(index)

      override def idiomReturningCapability: ReturningCapability    = parent.idiomReturningCapability
      override def productAggregationToken: ProductAggregationToken = parent.productAggregationToken
    }

  case class InsertUpdateStmt(action: Statement, where: Statement)
  private[getquill] def withActionAlias(parentIdiom: SqlIdiom, action: Action, alias: Ident)(implicit
    strategy: NamingStrategy,
    idiomContext: IdiomContext
  ): InsertUpdateStmt = {
    val idiom = copyIdiom(parentIdiom, Some(alias))
    import idiom._

    implicit val stableTokenizer = idiom.astTokenizer(
      new Tokenizer[Ast] {
        override def token(v: Ast): Token = astTokenizer(this, strategy, idiomContext).token(v)
      },
      strategy,
      idiomContext
    )

    action match {
      case Update(Filter(table: Entity, x, where), assignments) =>
        // Uses the `alias` passed in as `actionAlias` since that is now assigned to the copied SqlIdiom
        InsertUpdateStmt(
          // back here
          stmt"UPDATE ${table.token}${` AS [table]`} SET ${assignments.token}",
          stmt"${where.token}"
        )
      case Delete(Filter(table: Entity, x, where)) =>
        InsertUpdateStmt(
          stmt"DELETE FROM ${table.token}${` AS [table]`}",
          stmt"${where.token}"
        )
      case _ =>
        fail("Invalid state. Only UPDATE/DELETE with filter allowed here.")
    }
  }

  /**
   * Construct a new instance of the specified idiom with `newActionAlias`
   * variable specified so that actions (i.e. insert, and update) will be
   * rendered with the specified alias. This is needed for RETURNING clauses
   * that have queries inside. See #1509 for details.
   */
  private[getquill] def withActionAlias(parentIdiom: SqlIdiom, query: ReturningAction)(implicit
    strategy: NamingStrategy,
    idiomContext: IdiomContext
  ) = {
    val idiom = copyIdiom(parentIdiom, Some(query.alias))
    import idiom._

    implicit val stableTokenizer = idiom.astTokenizer(
      new Tokenizer[Ast] {
        override def token(v: Ast): Token = astTokenizer(this, strategy, idiomContext).token(v)
      },
      strategy,
      idiomContext
    )

    def ` AS [alias]`(alias: Ident) =
      useActionTableAliasAs match {
        case ActionTableAliasBehavior.UseAs  => stmt" AS ${alias.name.token}"
        case ActionTableAliasBehavior.SkipAs => stmt" ${alias.name.token}"
        case ActionTableAliasBehavior.Hide   => emptyStatement
      }

    query match {
      case r @ ReturningAction(Insert(table: Entity, Nil), alias, prop) =>
        stmt"INSERT INTO ${table.token}${` AS [alias]`(alias)} ${defaultAutoGeneratedToken(prop.token)} RETURNING ${tokenizeReturningClause(r)}"
      case r @ ReturningAction(action, alias, prop) =>
        stmt"${action.token} RETURNING ${tokenizeReturningClause(r)}"
    }
  }
}
