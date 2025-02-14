package io.getquill.norm.capture

import io.getquill.StatefulCache
import io.getquill.StatefulCache.NoCache
import io.getquill.ast.{IdentName, Query}
import io.getquill.util.TraceConfig

// This is not actually used anywhere but there is a test that tests
// the general functionality of dealiasing. Move this to there
object AvoidCapture {

  def apply(q: Query, cache: StatefulCache[Set[IdentName]], traceConfig: TraceConfig): Query =
    Dealias(AvoidAliasConflict(q, false, cache, traceConfig))(traceConfig, NoCache)
}
