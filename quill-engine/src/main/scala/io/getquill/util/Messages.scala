package io.getquill.util

import io.getquill.AstPrinter
import scala.collection.mutable.{ Map => MutableMap }

object Messages {

  private def variable(propName: String, envName: String, default: String) =
    Option(System.getProperty(propName)).orElse(sys.env.get(envName)).getOrElse(default)

  def resetCache(): Unit = cacheMap.clear()
  private val cacheMap: MutableMap[String, Any] = MutableMap()
  private def cache[T](name: String, value: => T): T =
    cacheMap.getOrElseUpdate(name, value).asInstanceOf[T]

  def quatKryoPoolSize = cache("quill.quat.kryoPool", variable("quill.quat.kryoPool", "quill_quat_kryoPool", "10").toInt)
  def maxQuatFields = cache("quill.quat.tooManyFields", variable("quill.quat.tooManyFields", "quill_quat_tooManyFields", "500").toInt)
  def strictQuatChecking = cache("quill.quat.strict", variable("quill.quat.strict", "quill_quat_strict", "false").toBoolean)
  def prettyPrint = cache("quill.macro.log.pretty", variable("quill.macro.log.pretty", "quill_macro_log", "false").toBoolean)
  def alwaysAlias = cache("quill.query.alwaysAlias", variable("quill.query.alwaysAlias", "quill_query_alwaysAlias", "false").toBoolean)
  def pruneColumns = cache("quill.query.pruneColumns", variable("quill.query.pruneColumns", "quill_query_pruneColumns", "true").toBoolean)
  def smartBooleans = cache("quill.query.smartBooleans", variable("quill.query.smartBooleans", "quill_query_smartBooleans", "true").toBoolean)
  def debugEnabled = cache("quill.macro.log", variable("quill.macro.log", "quill_macro_log", "true").toBoolean)
  def traceEnabled = cache("quill.trace.enabled", variable("quill.trace.enabled", "quill_trace_enabled", "false").toBoolean)
  def traceColors = cache("quill.trace.color", variable("quill.trace.color", "quill_trace_color,", "false").toBoolean)
  def traceOpinions = cache("quill.trace.opinion", variable("quill.trace.opinion", "quill_trace_opinion", "false").toBoolean)
  def traceAstSimple = cache("quill.trace.ast.simple", variable("quill.trace.ast.simple", "quill_trace_ast_simple", "false").toBoolean)
  def traceQuats = cache("quill.trace.quat", QuatTrace(variable("quill.trace.quat", "quill_trace_quat", QuatTrace.None.value)))
  def cacheDynamicQueries = cache("quill.query.cacheDaynamic", variable("quill.query.cacheDaynamic", "query_query_cacheDaynamic", "true").toBoolean)
  def querySubexpand = cache("quill.query.subexpand", variable("quill.query.subexpand", "query_query_subexpand", "true").toBoolean)
  def quillLogFile = cache("quill.log.file", LogToFile(variable("quill.log.file", "quill_log_file", "false")))

  sealed trait LogToFile
  object LogToFile {
    case class Enabled(file: String) extends LogToFile
    case object Disabled extends LogToFile
    def apply(switch: String): LogToFile =
      switch.trim match {
        case "false" => Disabled
        case other   => Enabled(other)
      }
  }

  sealed trait QuatTrace { def value: String }
  object QuatTrace {
    case object Short extends QuatTrace { val value = "short" }
    case object Full extends QuatTrace { val value = "full" }
    case object All extends QuatTrace { val value = "all" }
    case object None extends QuatTrace { val value = "none" }
    val values = List(Short, Full, All, None)
    def apply(str: String): QuatTrace =
      values.find(_.value == str).getOrElse(throw new IllegalArgumentException(s"The value ${str} is an invalid quat trace setting. Value values are: ${values.map(_.value).mkString(",")}"))
  }

  private[util] def traces: List[TraceType] = {
    val argValue = variable("quill.trace.types", "quill_trace_types", "standard")
    cache("quill.trace.types", {
      if (argValue == "all")
        TraceType.values
      else
        argValue
          .split(",")
          .toList
          .map(_.trim)
          .flatMap(trace => TraceType.values.filter(traceType => trace == traceType.value))
    })
  }

  def tracesEnabled(tt: TraceType) =
    (traceEnabled && traces.contains(tt)) || tt == TraceType.Warning

  def enableTrace(color: Boolean = true, quatTrace: QuatTrace = QuatTrace.Full, traceTypes: List[TraceType] = List(TraceType.SqlNormalizations, TraceType.Standard)): Unit = {
    System.setProperty("quill.trace.enabled", "true")
    System.setProperty("quill.trace.color", color.toString)
    System.setProperty("quill.trace.quat", quatTrace.value)
    System.setProperty("quill.trace.types", traceTypes.map(_.value).mkString(","))
    resetCache()
    ()
  }

  sealed trait TraceType { def value: String }
  object TraceType {
    case object SqlNormalizations extends TraceType { val value = "sql" }
    case object ExpandDistinct extends TraceType { val value = "distinct" }
    case object Normalizations extends TraceType { val value = "norm" }
    case object Standard extends TraceType { val value = "standard" }
    case object NestedQueryExpansion extends TraceType { val value = "nest" }
    case object AvoidAliasConflict extends TraceType { val value = "alias" }
    case object ShealthLeaf extends TraceType { val value = "sheath" }
    case object ReifyLiftings extends TraceType { val value = "reify" }
    case object PatMatch extends TraceType { val value = "patmatch" }
    case object Quotation extends TraceType { val value = "quote" }
    case object RepropagateQuats extends TraceType { val value = "reprop" }
    case object RenameProperties extends TraceType { val value = "rename" }
    case object ApplyMap extends TraceType { val value = "applymap" }
    // Specifically for situations where what needs to be printed is a type of warning to the user as opposed to an expansion
    // This kind of trace is always on by default and does not need to be enabled by the user.
    case object Warning extends TraceType { val value = "warning" }
    case object ExprModel extends TraceType { val value = "exprmodel" }
    case object Meta extends TraceType { val value = "meta" }
    case object Execution extends TraceType { val value = "exec" }
    case object DynamicExecution extends TraceType { val value = "dynamicexec" }
    case object Elaboration extends TraceType { val value = "elab" }

    def values: List[TraceType] = List(
      Standard, SqlNormalizations, Normalizations, NestedQueryExpansion, AvoidAliasConflict, ReifyLiftings, PatMatch, Quotation,
      RepropagateQuats, RenameProperties, Warning, ShealthLeaf, ApplyMap, ExpandDistinct, ExprModel, Meta, Execution, DynamicExecution, Elaboration
    )
  }

  val qprint = new AstPrinter(traceOpinions, traceAstSimple, Messages.traceQuats)
  def qprintCustom(traceOpinions: Boolean = false, traceAstSimple: Boolean = false, traceQuats: QuatTrace = QuatTrace.None) =
    new AstPrinter(traceOpinions, traceAstSimple, Messages.traceQuats)

  def fail(msg: String) =
    throw new IllegalStateException(msg)

  def title[T](label: String, traceType: TraceType = TraceType.Standard) =
    trace[T](("=".repeat(10)) + s" $label " + ("=".repeat(10)), 0, traceType)

  def trace[T](label: String, numIndent: Int = 0, traceType: TraceType = TraceType.Standard) =
    (v: T) =>
      {
        val indent = (0 to numIndent).map(_ => "").mkString("  ")
        if (tracesEnabled(traceType))
          println(s"$indent$label\n${
            {
              if (traceColors) qprint.apply(v).render else qprint.apply(v).plainText
            }.split("\n").map(s"$indent  " + _).mkString("\n")
          }")
        v
      }

  implicit class StringExt(str: String) {
    def repeat(n: Int) = (0 until n).map(_ => str).mkString
  }
}
