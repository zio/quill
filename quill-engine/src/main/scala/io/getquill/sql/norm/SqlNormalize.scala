package io.getquill.context.sql.norm

import io.getquill.StatelessCache
import io.getquill.norm.{SimplifyNullChecks, _}
import io.getquill.ast.{Ast, CollectStats, QueryStats}
import io.getquill.norm.ConcatBehavior.AnsiConcat
import io.getquill.norm.EqualityBehavior.AnsiEquality
import io.getquill.norm.capture.{AvoidAliasConflict, DemarcateExternalAliases}
import io.getquill.util.Messages.{TraceType, title}
import io.getquill.util.TraceConfig

object SqlNormalize {
  def apply(
    ast: Ast,
    transpileConfig: TranspileConfig,
    caches: SqlNormalizeCaches,
    concatBehavior: ConcatBehavior = AnsiConcat,
    equalityBehavior: EqualityBehavior = AnsiEquality
  ) =
    new SqlNormalize(concatBehavior, equalityBehavior, transpileConfig, caches)(ast)
}

case class CompilePhaseCaches(
  expandJoinCache: StatelessCache,
  renamePropertiesCache: StatelessCache,
  expandDistinctCache: StatelessCache,
  flattenOptionCache: StatelessCache,
  simplifyNullChecksCache: StatelessCache
)
object CompilePhaseCaches {
  val NoCache =
    CompilePhaseCaches(
      StatelessCache.NoCache,
      StatelessCache.NoCache,
      StatelessCache.NoCache,
      StatelessCache.NoCache,
      StatelessCache.NoCache
    )

  def unlimitedCache() =
    CompilePhaseCaches(
      StatelessCache.Unlimited(),
      StatelessCache.Unlimited(),
      StatelessCache.Unlimited(),
      StatelessCache.Unlimited(),
      StatelessCache.Unlimited()
    )
}

trait SqlNormalizeCaches {
  def normCaches: NormalizeCaches
  def phaseCaches: CompilePhaseCaches
}
object SqlNormalizeCaches {
  val NoCache =
    new SqlNormalizeCaches {
      val normCaches  = NormalizeCaches.NoCache
      val phaseCaches = CompilePhaseCaches.NoCache
    }

  def unlimitedLocal() =
    new SqlNormalizeCaches {
      val normCaches  = NormalizeCaches.unlimitedCache()
      val phaseCaches = CompilePhaseCaches.unlimitedCache()
    }
}

class SqlNormalize(
  concatBehavior: ConcatBehavior,
  equalityBehavior: EqualityBehavior,
  transpileConfig: TranspileConfig,
  caches: SqlNormalizeCaches
) {

  val sqlNormCaches  = caches.phaseCaches
  val NormalizePhase = new Normalize(caches.normCaches, transpileConfig)
  val traceConfig    = transpileConfig.traceConfig

  private def demarcate(heading: String) =
    ((ast: Ast) => title(heading, TraceType.SqlNormalizations)(ast))

  val ExpandJoinPhase       = new ExpandJoin(sqlNormCaches.expandJoinCache, NormalizePhase)
  val RenamePropertiesPhase = new RenameProperties(sqlNormCaches.renamePropertiesCache, traceConfig) // can't really cache this because renames are not on the quat comparison
  val ExpandDistinctPhase   = new ExpandDistinct(sqlNormCaches.expandDistinctCache, traceConfig)
  // TODO want to get rid of this stage
  val SheathLeafClausesPhase      = new SheathLeafClausesApply(traceConfig)
  val FlattenOptionOperationPhase = new FlattenOptionOperation(sqlNormCaches.flattenOptionCache, concatBehavior, transpileConfig.traceConfig)
  val SimplifyNullChecksPhase     = new SimplifyNullChecks(sqlNormCaches.simplifyNullChecksCache, equalityBehavior)

  private def normalize(stats: QueryStats) =
    (identity[Ast] _)
      .andThen(demarcate("original"))
      .andThen(if (stats.hasReturning) DemarcateExternalAliases.apply _ else identity[Ast] _)
      .andThen(demarcate("DemarcateReturningAliases"))
      .andThen(if (stats.hasOptionOps) FlattenOptionOperationPhase.apply _ else identity[Ast] _)
      .andThen(demarcate("FlattenOptionOperation"))
      .andThen(if (stats.hasOptionOps || stats.hasNullValues) SimplifyNullChecksPhase.apply _ else identity[Ast] _)
      .andThen(demarcate("SimplifyNullChecks"))
      .andThen(NormalizePhase.apply _)
      .andThen(demarcate("Normalize"))
      // Need to do RenameProperties before ExpandJoin which normalizes-out all the tuple indexes
      // on which RenameProperties relies
      // .andThen(RenameProperties.apply _)
      .andThen(if (stats.hasRenames) RenamePropertiesPhase.apply _ else identity[Ast] _)
      .andThen(demarcate("RenameProperties"))
      .andThen { ast =>
        if (stats.hasOptionOps) {
          val expanded = ExpandDistinctPhase(ast)
          demarcate("ExpandDistinct")
          // Normalize is only needed only because ExpandDistinct introduces an alias.
          // why are two normalize phases needed here???
          val e1 = NormalizePhase(expanded)
          demarcate("Normalize")
          val e2 = NormalizePhase(expanded)
          demarcate("Normalize")
          e2
        } else
          ast
      }
      .andThen(if (stats.hasJoins) ExpandJoinPhase.apply _ else identity[Ast] _)
      .andThen(demarcate("ExpandJoin"))
      .andThen(ExpandMappedInfix.apply _) // TODO disable if this has no infixes
      .andThen(if (stats.hasInfixes) demarcate("ExpandMappedInfix") else identity[Ast] _)
      .andThen(SheathLeafClausesPhase.apply _)
      .andThen(demarcate("SheathLeaves"))
      .andThen { ast =>
        // In the final stage of normalization, change all temporary aliases into
        // shorter ones of the form x[0-9]+.
        NormalizePhase.apply(AvoidAliasConflict.Ast(ast, true, caches.normCaches.avoidAliasCache, transpileConfig.traceConfig))
      }
      .andThen(demarcate("Normalize"))

  def apply(ast: Ast) = {
    val (stableAst, state) = StabilizeLifts.stabilize(ast)
    val stats              = CollectStats(stableAst)
    val outputAst          = normalize(stats)(stableAst)
    StabilizeLifts.revert(outputAst, state)
  }
}
