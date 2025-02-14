package io.getquill

import io.getquill.ast.{Ast, Query, StatefulTransformer}
import io.getquill.quat.Quat

import scala.collection.mutable

trait StatefulCache[State] {
  def getOrCache(q: Ast, s: State, default: => (Ast, StatefulTransformer[State])): (Ast, StatefulTransformer[State])
  def getOrCache(q: Query, s: State, default: => (Query, StatefulTransformer[State])): (Query, StatefulTransformer[State])
}
object StatefulCache {

  private val noCacheInstance =
    new StatefulCache[Any] {
      override def getOrCache(q: Ast, s: Any, default: => (Ast, StatefulTransformer[Any])): (Ast, StatefulTransformer[Any])       = default
      override def getOrCache(q: Query, s: Any, default: => (Query, StatefulTransformer[Any])): (Query, StatefulTransformer[Any]) = default
    }

  // Since the sateless NoCache[T] just returns what it was passed in we can reuse the same instance
  // (and typecast with impunity) since there is no internal logic
  def NoCache[T]: StatefulCache[T] = noCacheInstance.asInstanceOf[StatefulCache[T]]

  // Make sure to store the Quat of the Ast since we don't want to cache the same Ast with different Quats
  // e.g. Cache ident(x, Quat(Person)) should not be the same as ident(x, Quat(Address)) in the caching process
  // because incorrect queries will be generated.
  case class Unlimited[State](
    astMap: mutable.Map[(Ast, Quat, State), (Ast, StatefulTransformer[State])] = mutable.Map.empty[(Ast, Quat, State), (Ast, StatefulTransformer[State])],
    queryMap: mutable.Map[(Query, Quat, State), (Query, StatefulTransformer[State])] = mutable.Map.empty[(Query, Quat, State), (Query, StatefulTransformer[State])]
  ) extends StatefulCache[State] {
    override def getOrCache(q: Ast, s: State, default: => (Ast, StatefulTransformer[State])): (Ast, StatefulTransformer[State]) = {
      val key = (q, q.quat, s)
      astMap.get(key) match {
        case Some(value) => value
        case None =>
          val (ast, transformer) = default
          astMap.put(key, (ast, transformer))
          (ast, transformer)
      }
    }
    override def getOrCache(q: Query, s: State, default: => (Query, StatefulTransformer[State])): (Query, StatefulTransformer[State]) = {
      val key = (q, q.quat, s)
      queryMap.get(key) match {
        case Some(value) => value
        case None =>
          val (query, transformer) = default
          queryMap.put(key, (query, transformer))
          (query, transformer)
      }
    }
  }
}

trait StatelessCache {
  def getOrCache(q: Ast, default: => Ast): Ast
  def getOrCache(q: Query, default: => Query): Query
}
object StatelessCache {
  object NoCache extends StatelessCache {
    override def getOrCache(q: Ast, default: => Ast): Ast       = default
    override def getOrCache(q: Query, default: => Query): Query = default
  }
  case class Unlimited(
    astCache: mutable.Map[(Ast, Quat), Ast] = mutable.Map.empty,
    queryCache: mutable.Map[(Query, Quat), Query] = mutable.Map.empty
  ) extends StatelessCache {
    override def getOrCache(q: Ast, default: => Ast): Ast = {
      val key = (q, q.quat)
      astCache.get(key) match {
        case Some(value) => value
        case None =>
          val defaultVal = default
          astCache.put(key, defaultVal)
          defaultVal
      }
    }
    override def getOrCache(q: Query, default: => Query): Query = {
      val key = (q, q.quat)
      queryCache.get(key) match {
        case Some(value) => value
        case None =>
          val defaultVal = default
          queryCache.put(key, defaultVal)
          defaultVal
      }
    }
  }
}

trait HasStatelessCache {
  def cache: StatelessCache
  def cached(q: Ast)(default: => Ast): Ast       = cache.getOrCache(q, default)
  def cached(q: Query)(default: => Query): Query = cache.getOrCache(q, default)
}

trait StatelessCacheOpt {
  def getOrCache(q: Ast, default: => Option[Ast]): Option[Ast]
  def getOrCache(q: Query, default: => Option[Query]): Option[Query]
}
object StatelessCacheOpt {
  object NoCache extends StatelessCacheOpt {
    override def getOrCache(q: Ast, default: => Option[Ast]): Option[Ast]       = default
    override def getOrCache(q: Query, default: => Option[Query]): Option[Query] = default
  }
  case class Unlimited(
    astCache: mutable.Map[(Ast, Quat), Option[Ast]] = mutable.Map.empty,
    queryCache: mutable.Map[(Query, Quat), Option[Query]] = mutable.Map.empty
  ) extends StatelessCacheOpt {
    override def getOrCache(q: Ast, default: => Option[Ast]): Option[Ast] = {
      val key = (q, q.quat)
      astCache.get(key) match {
        case Some(value) => value
        case None =>
          val defaultVal = default
          astCache.put(key, defaultVal)
          defaultVal
      }
    }

    override def getOrCache(q: Query, default: => Option[Query]): Option[Query] = {
      val key = (q, q.quat)
      queryCache.get(key) match {
        case Some(value) => value
        case None =>
          val defaultVal = default
          queryCache.put(key, defaultVal)
          defaultVal
      }
    }
  }
}

trait HasStatelessCacheOpt {
  def cache: StatelessCacheOpt
  def cached(q: Ast)(default: => Option[Ast]): Option[Ast]       = cache.getOrCache(q, default)
  def cached(q: Query)(default: => Option[Query]): Option[Query] = cache.getOrCache(q, default)
}

trait HasStatefulCache[T] {
  def cache: StatefulCache[T]
  def state: T

  def cached(q: Ast)(default: => (Ast, StatefulTransformer[T])): (Ast, StatefulTransformer[T]) =
    cache.getOrCache(
      q,
      this.state,
      // If cache is a miss, evaluate "default" and write it together with the state to the cache
      default
    )

  def cached(q: Query)(default: => (Query, StatefulTransformer[T])): (Query, StatefulTransformer[T]) =
    cache.getOrCache(
      q,
      this.state,
      // If cache is a miss, evaluate "default" and write it together with the state to the cache
      default
    )
}
