package zio.logging

import zio.{FiberRef, Runtime, Scope, UIO, Unsafe, ZIO, ZLogger}

import java.nio.charset.Charset
import java.nio.file.Path

object extensions {

  /**
   * This code is basically an inlined version of the [[fileLogger]] code from
   * which we removed the ZLayer part which is getting in our way. We don't need
   * layers here anyway.
   */
  def executeWithLogger(config: FileLoggerConfig)(zio: UIO[Unit]): Unit =
    Unsafe.unsafe { implicit u =>
      Runtime.default.unsafe.run {
        ZIO.scoped {
          for {
            logger <- makeFileLogger(config)
            _      <- FiberRef.currentLoggers.locallyScopedWith(_ + logger)
            _      <- zio
          } yield ()
        }
      }.getOrThrow()
    }

  /**
   * Copied from zio-logging internals.
   *
   * We need to copy the function as it's private and we can't access it
   *
   * See:
   *   - https://github.com/zio/zio-logging/blob/v2.1.14/core/shared/src/main/scala/zio/logging/package.scala#L459-L468
   */
  private def makeFileLogger(config: FileLoggerConfig): ZIO[Scope, Throwable, ZLogger[String, Any]] =
    makeFileLogger(
      config.destination,
      config.format.toLogger,
      config.filter.toFilter,
      config.charset,
      config.autoFlushBatchSize,
      config.bufferedIOSize,
      config.rollingPolicy
    )

  /**
   * Adapted from zio-logging internals
   *
   *   1. We need to copy the function as it's private and we can't access it.
   *      2. We wrapped the code we copied into a ZIO to be sure to correctly
   *         close the writer.
   *
   * See:
   *   - https://github.com/zio/zio-logging/blob/v2.1.14/core/shared/src/main/scala/zio/logging/package.scala#L470-L491
   */
  private def makeFileLogger(
    destination: Path,
    logger: ZLogger[String, String],
    logFilter: LogFilter[String],
    charset: Charset,
    autoFlushBatchSize: Int,
    bufferedIOSize: Option[Int],
    rollingPolicy: Option[FileLoggerConfig.FileRollingPolicy]
  ): ZIO[Scope, Throwable, ZLogger[String, Any]] =
    for {
      logWriter <- ZIO.acquireRelease {
                     ZIO.attempt(
                       new zio.logging.internal.FileWriter(
                         destination,
                         charset,
                         autoFlushBatchSize,
                         bufferedIOSize,
                         rollingPolicy
                       )
                     )
                   }(writer => ZIO.succeed(writer.close()))
      stringLogger = logFilter.filter(logger.map { (line: String) =>
                       try logWriter.writeln(line)
                       catch {
                         case t: VirtualMachineError => throw t
                         case _: Throwable           => ()
                       }
                     })
    } yield stringLogger

}
