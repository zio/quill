package io.getquill

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Try}

object MirrorContexts {

  object testContext extends TestMirrorContextTemplate(MirrorIdiom, Literal) with TestEntities
  object testAsyncContext extends AsyncMirrorContext(MirrorIdiom, Literal) with TestEntities {

    // hack to avoid Await.result since scala.js doesn't support it
    implicit val immediateEC: ExecutionContext = new ExecutionContext {
      def execute(runnable: Runnable)     = runnable.run()
      def reportFailure(cause: Throwable) = ()
    }

    def eval[T](f: Future[T]): T = {
      var res: Try[T] = Failure(new IllegalStateException())
      f.onComplete(res = _)
      res.get
    }
  }

}
