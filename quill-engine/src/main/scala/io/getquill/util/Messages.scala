package io.getquill.util

import io.getquill.AstPrinter
import scala.collection.mutable.{Map => MutableMap}

object Messages {

  private def variable(propName: String, envName: String, default: String) =
    Option(System.getProperty(propName)).orElse(sys.env.get(envName)).getOrElse(default)

  def resetCache(): Unit                        = cacheMap.clear()
  private val cacheMap: MutableMap[String, Any] = MutableMap()
  private def cache[T](name: String, value: => T): T =
    cacheMap.getOrElseUpdate(name, value).asInstanceOf[T]

  def quatKryoPoolSize =
    cache("quill.quat.kryoPool", variable("quill.quat.kryoPool", "quill_quat_kryoPool", "10").toInt)
  def maxQuatFields =
    cache("quill.quat.tooManyFields", variable("quill.quat.tooManyFields", "quill_quat_tooManyFields", "500").toInt)
  def attachTopLevelQuats = cache(
    "quill.quat.attachTopLevel",
    variable("quill.quat.attachTopLevel", "quill_quat_attachTopLevel", "true").toBoolean
  )
  def strictQuatChecking =
    cache("quill.quat.strict", variable("quill.quat.strict", "quill_quat_strict", "false").toBoolean)
  def prettyPrint =
    cache("quill.macro.log.pretty", variable("quill.macro.log.pretty", "quill_macro_log", "false").toBoolean)
  def alwaysAlias =
    cache("quill.query.alwaysAlias", variable("quill.query.alwaysAlias", "quill_query_alwaysAlias", "false").toBoolean)
  def pruneColumns = cache(
    "quill.query.pruneColumns",
    variable("quill.query.pruneColumns", "quill_query_pruneColumns", "true").toBoolean
  )
  def smartBooleans = cache(
    "quill.query.smartBooleans",
    variable("quill.query.smartBooleans", "quill_query_smartBooleans", "true").toBoolean
  )
  def debugEnabled = cache("quill.macro.log", variable("quill.macro.log", "quill_macro_log", "true").toBoolean)
  def traceEnabled =
    cache("quill.trace.enabled", variable("quill.trace.enabled", "quill_trace_enabled", "false").toBoolean)
  def traceColors = cache("quill.trace.color", variable("quill.trace.color", "quill_trace_color,", "false").toBoolean)
  def traceOpinions =
    cache("quill.trace.opinion", variable("quill.trace.opinion", "quill_trace_opinion", "false").toBoolean)
  def traceAstSimple =
    cache("quill.trace.ast.simple", variable("quill.trace.ast.simple", "quill_trace_ast_simple", "false").toBoolean)
  def traceQuats =
    cache("quill.trace.quat", QuatTrace(variable("quill.trace.quat", "quill_trace_quat", QuatTrace.None.value)))

  def cacheNormalization = cache(
    "quill.cache.norm",
    variable("quill.cache.norm", "quill_cache_norm", "true").toBoolean
  )

  def cacheDynamicQueries = cache(
    "quill.query.cacheDynamic",
    variable("quill.query.cacheDynamic", "query_query_cacheDynamic", "true").toBoolean
  )
  def cacheDynamicMaxSize = cache(
    "quill.query.cacheDynamicMaxSize",
    variable("quill.query.cacheDynamicMaxSize", "query_query_cacheDynamicMaxSize", "1024").toLong
  )
  def querySubexpand =
    cache("quill.query.subexpand", variable("quill.query.subexpand", "query_query_subexpand", "true").toBoolean)
  def quillLogFile = cache("quill.log.file", LogToFile(variable("quill.log.file", "quill_log_file", "false")))
  def errorDetail  = cache("quill.error.detail", variable("quill.error.detail", "quill_error_detail", "false").toBoolean)
  def disableReturning = cache(
    "quill.query.disableReturning",
    variable("quill.query.disableReturning", "quill_query_disableReturning", "false").toBoolean
  )
  def logBinds = cache("quill.binds.log", variable("quill.binds.log", "quill_binds_log", "false").toBoolean)
  def queryTooLongForLogs =
    cache("quill.query.tooLong", variable("quill.query.tooLong", "quill_query_tooLong", "200").toInt)
  def errorPrefix = cache("quill.error.prefix", variable("quill.error.prefix", "quill_error_prefix", "false").toBoolean)

  sealed trait LogToFile
  object LogToFile {
    case class Enabled(file: String) extends LogToFile
    case object Disabled             extends LogToFile
    def apply(switch: String): LogToFile =
      switch.trim match {
        case "false" => Disabled
        case other   => Enabled(other)
      }
  }

  sealed trait QuatTrace { def value: String }
  object QuatTrace {
    case object Short extends QuatTrace { val value = "short" }
    case object Full  extends QuatTrace { val value = "full"  }
    case object All   extends QuatTrace { val value = "all"   }
    case object None  extends QuatTrace { val value = "none"  }
    val values: List[QuatTrace] = List(Short, Full, All, None)
    def apply(str: String): QuatTrace =
      values
        .find(_.value == str)
        .getOrElse(
          throw new IllegalArgumentException(
            s"The value ${str} is an invalid quat trace setting. Value values are: ${values.map(_.value).mkString(",")}"
          )
        )
  }

  private[getquill] def traces: List[TraceType] = {
    val argValue = variable("quill.trace.types", "quill_trace_types", "standard")
    cache(
      "quill.trace.types",
      if (argValue == "all")
        TraceType.values
      else
        argValue
          .split(",")
          .toList
          .map(_.trim)
          .flatMap(trace => TraceType.values.filter(traceType => trace == traceType.value))
    )
  }

  def tracesEnabled(tt: TraceType): Boolean =
    (traceEnabled && traces.contains(tt)) || tt == TraceType.Warning

  def enableTrace(
    color: Boolean = true,
    quatTrace: QuatTrace = QuatTrace.Full,
    traceTypes: List[TraceType] = List(TraceType.SqlNormalizations, TraceType.Standard)
  ): Unit = {
    System.setProperty("quill.trace.enabled", "true")
    System.setProperty("quill.trace.color", color.toString)
    System.setProperty("quill.trace.quat", quatTrace.value)
    System.setProperty("quill.trace.types", traceTypes.map(_.value).mkString(","))
    resetCache()
    ()
  }

  sealed trait TraceType { def value: String }
  object TraceType       {
    // Note: We creates types as well as traits so these can be referred to easily by type-name (i.e. without .type)
    //       this is useful when using the EnableTrace implicit pattern i.e:
    //       implicit val t = new EnableTrace {  override type Trace = TraceType.Normalizations :: HNil }
    //       Otherwise it would have to be override type Trace = TraceType.Normalizations.type

    // Specifically for situations where what needs to be printed is a type of warning to the user as opposed to an expansion
    // This kind of trace is always on by default and does not need to be enabled by the user.
    sealed trait Warning                extends TraceType { val value = "warning"     }
    sealed trait SqlNormalizations      extends TraceType { val value = "sql"         }
    sealed trait ExpandDistinct         extends TraceType { val value = "distinct"    }
    sealed trait Normalizations         extends TraceType { val value = "norm"        }
    sealed trait Standard               extends TraceType { val value = "standard"    }
    sealed trait NestedQueryExpansion   extends TraceType { val value = "nest"        }
    sealed trait AvoidAliasConflict     extends TraceType { val value = "alias"       }
    sealed trait ShealthLeaf            extends TraceType { val value = "sheath"      }
    sealed trait ReifyLiftings          extends TraceType { val value = "reify"       }
    sealed trait PatMatch               extends TraceType { val value = "patmatch"    }
    sealed trait Quotation              extends TraceType { val value = "quote"       }
    sealed trait RepropagateQuats       extends TraceType { val value = "reprop"      }
    sealed trait RenameProperties       extends TraceType { val value = "rename"      }
    sealed trait ApplyMap               extends TraceType { val value = "applymap"    }
    sealed trait ExprModel              extends TraceType { val value = "exprmodel"   }
    sealed trait Meta                   extends TraceType { val value = "meta"        }
    sealed trait Execution              extends TraceType { val value = "exec"        }
    sealed trait DynamicExecution       extends TraceType { val value = "dynamicexec" }
    sealed trait Elaboration            extends TraceType { val value = "elab"        }
    sealed trait SqlQueryConstruct      extends TraceType { val value = "sqlquery"    }
    sealed trait FlattenOptionOperation extends TraceType { val value = "option"      }
    sealed trait Particularization      extends TraceType { val value = "parti"       }

    object Warning                extends Warning
    object SqlNormalizations      extends SqlNormalizations
    object ExpandDistinct         extends ExpandDistinct
    object Normalizations         extends Normalizations
    object Standard               extends Standard
    object NestedQueryExpansion   extends NestedQueryExpansion
    object AvoidAliasConflict     extends AvoidAliasConflict
    object ShealthLeaf            extends ShealthLeaf
    object ReifyLiftings          extends ReifyLiftings
    object PatMatch               extends PatMatch
    object Quotation              extends Quotation
    object RepropagateQuats       extends RepropagateQuats
    object RenameProperties       extends RenameProperties
    object ApplyMap               extends ApplyMap
    object ExprModel              extends ExprModel
    object Meta                   extends Meta
    object Execution              extends Execution
    object DynamicExecution       extends DynamicExecution
    object Elaboration            extends Elaboration
    object SqlQueryConstruct      extends SqlQueryConstruct
    object FlattenOptionOperation extends FlattenOptionOperation
    object Particularization      extends Particularization

    def values: List[TraceType] = List(
      Standard,
      SqlNormalizations,
      Normalizations,
      NestedQueryExpansion,
      AvoidAliasConflict,
      ReifyLiftings,
      PatMatch,
      Quotation,
      RepropagateQuats,
      RenameProperties,
      Warning,
      ShealthLeaf,
      ApplyMap,
      ExpandDistinct,
      ExprModel,
      Meta,
      Execution,
      DynamicExecution,
      Elaboration,
      SqlQueryConstruct,
      FlattenOptionOperation,
      Particularization
    )
  }

  val qprint = new AstPrinter(traceOpinions, traceAstSimple, Messages.traceQuats)
  def qprintCustom(
    traceOpinions: Boolean = false,
    traceAstSimple: Boolean = false,
    traceQuats: QuatTrace = QuatTrace.None
  ) =
    new AstPrinter(traceOpinions, traceAstSimple, traceQuats)

  def fail(msg: String): Nothing =
    throw new IllegalStateException(msg)

  def title[T](label: String, traceType: TraceType = TraceType.Standard): T => T =
    trace[T](("=".repeat(10)) + s" $label " + ("=".repeat(10)), 0, traceType)

  def trace[T](label: String, numIndent: Int = 0, traceType: TraceType = TraceType.Standard): T => T =
    (v: T) => {
      val indent = (0 to numIndent).map(_ => "").mkString("  ")
      if (tracesEnabled(traceType))
        println(s"$indent$label\n${{
            if (traceColors) qprint.apply(v).render else qprint.apply(v).plainText
          }.split("\n").map(s"$indent  " + _).mkString("\n")}")
      v
    }

  implicit final class StringExt(private val str: String) extends AnyVal {
    def repeat(n: Int): String = (0 until n).map(_ => str).mkString
  }
}
