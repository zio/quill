package io.getquill.norm

import io.getquill.{HasStatelessCache, StatefulCache, StatelessCache, StatelessCacheOpt}
import io.getquill.ast.{Action, Ast, Ident, IdentName, Query, StatelessTransformer}
import io.getquill.norm.capture.{AvoidAliasConflictApply, DealiasApply}
import io.getquill.util.Interpolator
import io.getquill.util.Messages.{TraceType, title, trace}
import io.getquill.util.Messages.TraceType.Normalizations

import scala.annotation.tailrec

case class NormalizeCaches(
  mainCache: StatelessCache,
  dealiasCache: StatefulCache[Option[Ident]],
  avoidAliasCache: StatefulCache[Set[IdentName]],
  normalizeNestedCache: StatelessCacheOpt,
  symbolicReductionCache: StatelessCacheOpt,
  adHocReductionCache: StatelessCacheOpt,
  orderTermsCache: StatelessCacheOpt
)
object NormalizeCaches {
  val NoCache =
    NormalizeCaches(
      StatelessCache.NoCache,
      StatefulCache.NoCache,
      StatefulCache.NoCache,
      StatelessCacheOpt.NoCache,
      StatelessCacheOpt.NoCache,
      StatelessCacheOpt.NoCache,
      StatelessCacheOpt.NoCache
    )

  def unlimitedCache() =
    NormalizeCaches(
      StatelessCache.NoCache,
      StatefulCache.Unlimited[Option[Ident]](),
      StatefulCache.Unlimited[Set[IdentName]](),
      StatelessCacheOpt.Unlimited(),
      StatelessCacheOpt.Unlimited(),
      StatelessCacheOpt.Unlimited(),
      StatelessCacheOpt.Unlimited()
    )
}

// TODO cache this whole thing? (need to stablize lifts first)
//      perhaps rename this to NormalizeUnsafe, the safe version always stablizes the lifts first
class Normalize(caches: NormalizeCaches, config: TranspileConfig) extends StatelessTransformer with HasStatelessCache {
  val cache: StatelessCache = caches.mainCache

  val traceConf = config.traceConfig
  val interp    = new Interpolator(TraceType.Normalizations, traceConf, 1)
  import interp._

  // These are the actual phases of core-normalization. Highlighting them as such.
  lazy val NormalizeReturningPhase   = new NormalizeReturning(this)
  lazy val DealiasPhase              = new DealiasApply(traceConf, caches.dealiasCache)
  lazy val AvoidAliasConflictPhase   = new AvoidAliasConflictApply(caches.avoidAliasCache, traceConf)
  val NormalizeNestedStructuresPhase = new NormalizeNestedStructures(this, caches.normalizeNestedCache)
  val SymbolicReductionPhase         = new SymbolicReduction(caches.symbolicReductionCache, traceConf)
  val AdHocReductionPhase            = new AdHocReduction(caches.adHocReductionCache, traceConf)
  val OrderTermsPhase                = new OrderTerms(caches.orderTermsCache, traceConf)

  override def apply(q: Ast): Ast = cached(q) {
    super.apply(BetaReduction(q))
  }

  override def apply(q: Action): Action =
    NormalizeReturningPhase(super.apply(q))

  override def apply(q: Query): Query = cached(q) {
    trace"Avoid Capture and Normalize $q into:" andReturn
      norm(DealiasPhase(AvoidAliasConflictPhase(q, false)))
  }

  private def traceNorm[T](label: String) =
    trace[T](s"${label} (Normalize)", 1, Normalizations)

  private def demarcate(heading: String) =
    ((ast: Query) => title(s"(Normalize) $heading", TraceType.Normalizations)(ast))

  // Create an ApplyMap phase-instance that will always return None if configured to be disabled.
  // This is a simple way of not running the phase at all if it is not configured to run.
  object ApplyMapPhase {
    // For logging that ApplyMap has been disabled
    val applyMapInterp   = new Interpolator(TraceType.ApplyMap, traceConf, 1)
    val applyMapInstance = new ApplyMap(traceConf)
    def unapply(query: Query): Option[Query] =
      if (config.disablePhases.contains(OptionalPhase.ApplyMap)) {
        import applyMapInterp._
        trace"ApplyMap phase disabled. Not executing on: $query".andLog()
        None
      } else {
        applyMapInstance.unapply(query)
      }
  }

  // TODO cache this? (need to stablize lifts first)
  @tailrec
  private def norm(q: Query): Query =
    q match {
      case NormalizeNestedStructuresPhase(query) =>
        demarcate("NormalizeNestedStructures")(query)
        norm(query)
      case ApplyMapPhase(query) =>
        demarcate("ApplyMap")(query)
        norm(query)
      case SymbolicReductionPhase(query) =>
        demarcate("SymbolicReduction")(query)
        norm(query)
      case AdHocReductionPhase(query) =>
        demarcate("AdHocReduction")(query)
        norm(query)
      case OrderTermsPhase(query) =>
        demarcate("OrderTerms")(query)
        norm(query)
      case other =>
        other
    }
}
