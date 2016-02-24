package io.getquill.sources.sql

import io.getquill.ast._
import io.getquill.norm.Normalize
import io.getquill.naming.NamingStrategy
import io.getquill.sources.sql.idiom.SqlIdiom
import io.getquill.util.Show._
import io.getquill.util.Messages._
import io.getquill.norm.capture.AvoidAliasConflict
import io.getquill.norm.capture.AvoidCapture
import io.getquill.norm.FlattenOptionOperation
import io.getquill.sources.sql.norm.{MergeGeneratedWithEntity, ExpandJoin, ExpandNestedQueries, MergeSecondaryJoin}

object Prepare {

  def apply(ast: Ast, params: List[Ident])(implicit d: SqlIdiom, n: NamingStrategy) = {
    import d._
    val (bindedAst, idents) = BindVariables(normalize(ast), params)
    val sqlString =
      bindedAst match {
        case q: Query =>
          val sql = SqlQuery(q)
          VerifySqlQuery(sql).map(fail)
          ExpandNestedQueries(sql, collection.Set.empty).show
        case other =>
          other.show
      }
    (sqlString, idents)
  }

  private[this] val normalize =
    (identity[Ast] _)
      .andThen(Normalize.apply _)
      .andThen(ExpandJoin.apply _)
      .andThen(MergeGeneratedWithEntity.apply _)
      .andThen(Normalize.apply _)
      .andThen(MergeSecondaryJoin.apply _)
      .andThen(FlattenOptionOperation.apply _)
}
