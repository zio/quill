package io.getquill.context.sql.norm

import io.getquill.{HasStatelessCache, StatelessCache}
import io.getquill.ast.Visibility.Hidden
import io.getquill.ast._
import io.getquill.quat.Quat
import io.getquill.quat.QuatNestingHelper._
import io.getquill.util.{Interpolator, TraceConfig}
import io.getquill.util.Messages.TraceType

class ExpandDistinct(val cache: StatelessCache, traceConfig: TraceConfig) extends HasStatelessCache {

  val interp = new Interpolator(TraceType.ExpandDistinct, traceConfig, 3)
  import interp._

  def apply(q: Ast): Ast = cached(q) {
    q match {
      case Distinct(q) =>
        trace"ExpandDistinct Distinct(inside)" andReturn
          Distinct(apply(q))

      case q =>
        Transform(q) {
          case Aggregation(op, Distinct(q)) =>
            trace"ExpandDistinct Agg(distinct)" andReturn
              Aggregation(op, Distinct(apply(q)))

          case Distinct(Map(q, x, cc @ Tuple(values))) =>
            val newIdent = Ident(x.name, valueQuat(cc.quat))
            trace"ExpandDistinct Distinct(Map(q, _, Tuple))" andReturn
              Map(
                Distinct(Map(q, x, cc)),
                newIdent,
                Tuple(values.zipWithIndex.map { case (_, i) =>
                  Property(newIdent, s"_${i + 1}")
                })
              )

          // Situations like this:
          //    case class AdHocCaseClass(id: Int, name: String)
          //    val q = quote {
          //      query[SomeTable].map(st => AdHocCaseClass(st.id, st.name)).distinct
          //    }
          // ... need some special treatment. Otherwise their values will not be correctly expanded.
          case Distinct(Map(q, x, cc @ CaseClass(n, values))) =>
            val newIdent = Ident(x.name, valueQuat(cc.quat))
            trace"ExpandDistinct Distinct(Map(q, _, CaseClass))" andReturn
              Map(
                Distinct(Map(q, x, cc)),
                newIdent,
                CaseClass(
                  n,
                  values.map { case (name, _) =>
                    (name, Property(newIdent, name))
                  }
                )
              )

          // Need some special handling to address issues with distinct returning a single embedded entity i.e:
          // query[Parent].map(p => p.emb).distinct.map(e => (e.name, e.id))
          // cannot treat such a case normally or "confused" queries will result e.g:
          // SELECT p.embname, p.embid FROM (SELECT DISTINCT emb.name /* Where the heck is 'emb' coming from? */ AS embname, emb.id AS embid FROM Parent p) AS p
          case d @ Distinct(Map(q, x, p @ Property.Opinionated(_, _, _, Hidden))) =>
            trace"ExpandDistinct Keep Distinct(Map(q, _, Property(Hidden)))" andReturn d

          // This was a buggy clause that was not well typed. Not needed anymore since we have SheathLeafClauses
          // In situations where a simple, single property is being distinct-ed it is not necessary to translate it into a single-element tuple
          // for example query[Person].map(p => p.name).distinct.map(p => (p.name, pname)) can be:
          // SELECT p.name, p.name FROM (SELECT DISTINCT p.name from Person p) AS p
          // This would normally become:
          // SELECT p._1, p._1 FROM (SELECT DISTINCT p.name AS _1 from Person p) AS p
          //          case Distinct(Map(q, x, p @ Property(id: Ident, name))) =>
          //            val innerDistinct = Distinct(Map(q, x, p))
          //            val newQuat = valueQuat(id.quat)
          //            trace"ExpandDistinct Distinct(Map(q, _, Property))" andReturn
          //              Map(innerDistinct, x, Property(id.copy(quat = newQuat), name))

          // Problems with distinct were first discovered in #1032. Basically, unless
          // the distinct is "expanded" adding an outer map, Ident's representing a Table will end up in invalid places
          // such as "ORDER BY tableIdent" etc...
          case Distinct(Map(q, x, p)) =>
            val newMap   = Map(q, x, Tuple(List(p)))
            val newQuat  = Quat.Tuple(valueQuat(p.quat)) // force quat recomputation for perf purposes
            val newIdent = Ident(x.name, newQuat)
            trace"ExpandDistinct Distinct(Map(other))" andReturn
              Map(Distinct(newMap), newIdent, Property(newIdent, "_1"))
        }
    }
  }
}
