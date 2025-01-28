package io.getquill.context.sql

import io.getquill.ReturnAction.{ReturnColumns, ReturnRecord}
import io.getquill._
import io.getquill.base.Spec
import io.getquill.context.mirror.Row

class SqlActionMacroSpec extends Spec {

  val testContext = io.getquill.context.sql.testContext

  "runs actions" - {
    import testContext._
    "without bindings" - {
      "update" in {
        val q = quote {
          qr1.filter(t => t.s == null).update(_.s -> "s")
        }
        testContext.run(q).string mustEqual
          "UPDATE TestEntity AS t SET s = 's' WHERE t.s IS NULL"
      }
      "insert" in {
        val q = quote {
          qr1.insert(_.s -> "s")
        }
        testContext.run(q).string mustEqual
          "INSERT INTO TestEntity (s) VALUES ('s')"
      }
      "delete" in {
        val q = quote {
          qr1.filter(t => t.s == null).delete
        }
        testContext.run(q).string mustEqual
          "DELETE FROM TestEntity AS t WHERE t.s IS NULL"
      }
    }
    "with bindings" - {
      "one" in {
        val q = quote {
          qr1.insert(_.s -> lift("s"))
        }
        val mirror = testContext.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s) VALUES (?)"
        mirror.prepareRow mustEqual Row("s")
      }
      "two" in {
        val q = quote {
          qr1.insert(_.s -> lift("s"), _.i -> lift(1))
        }
        val mirror = testContext.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i) VALUES (?, ?)"
        mirror.prepareRow mustEqual Row("s", 1)
      }
    }

    "insert returning generated" - {
      "single with returning generated" in {
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returningGenerated(_.l)
        }

        val mirror = testContext.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,o,b) VALUES (?, ?, ?, ?)"
        mirror.returningBehavior mustEqual ReturnColumns(List("l"))
      }
      "with assigned values" in {
        val q = quote {
          qr1.insert(_.s -> "s", _.i -> 0).returningGenerated(_.l)
        }

        val mirror = testContext.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i) VALUES ('s', 0)"
        mirror.returningBehavior mustEqual ReturnColumns(List("l"))
      }
      "single should fail on record type with multiple fields" in {
        """testContext.run(qr1.insert(lift(TestEntity("s", 0, 1L, None, true))).returningGenerated(r => r))""" mustNot compile
      }
      "multi" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returningGenerated(_.l)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,o,b) VALUES (?, ?, ?, ?)"
        mirror.returningBehavior mustEqual ReturnColumns(List("l"))
      }
      "multi with record type returning" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returning(r => r)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o,b) VALUES (?, ?, ?, ?, ?)"
        mirror.returningBehavior mustEqual ReturnColumns(List("s", "i", "l", "o", "b"))
      }
      "multi with record type returning generated should exclude all" in testContext.withDialect(
        MirrorSqlDialectWithReturnMulti
      ) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returningGenerated(r => r)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity DEFAULT VALUES"
        mirror.returningBehavior mustEqual ReturnColumns(List("s", "i", "l", "o", "b"))
      }
      "multi - should fail on operation" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
        """import ctx._; quote { qr1.insert(lift(TestEntity("s", 0, 1L, None))).returningGenerated(r => (r.i, r.l + 1)) }""" mustNot compile
      }
      "multi - should fail on operation inside case class" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) {
        ctx =>
          case class Foo(one: Int, two: Long)
          """import ctx._; quote { qr1.insert(lift(TestEntity("s", 0, 1L, None))).returningGenerated(r => Foo(r.i, r.l + 1)) }""" mustNot compile
      }
      "no return - should fail on property" in testContext.withDialect(MirrorSqlDialectWithNoReturn) { ctx =>
        """import ctx._; quote { qr1.insert(lift(TestEntity("s", 0, 1L, None))).returningGenerated(r => r.i) }""" mustNot compile
      }
      "returning clause - single" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returningGenerated(_.l)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,o,b) VALUES (?, ?, ?, ?) RETURNING l"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - multi" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, false))).returningGenerated(r => (r.i, r.l, r.b))
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,o) VALUES (?, ?) RETURNING i, l, b"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - operation" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returningGenerated(r => (r.i, r.l + 1))
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,o,b) VALUES (?, ?, ?) RETURNING i, l + 1"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - record" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returning(r => r)
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING s, i, l, o, b"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning many clause - record" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returningMany(r => r)
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING s, i, l, o, b"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning generated clause - record" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returningGenerated(r => r)
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity DEFAULT VALUES RETURNING s, i, l, o, b"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - embedded" - {
        case class Dummy(i: Int)

        "embedded property" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx => //
          import ctx._
          val q = quote {
            qr1Emb.insertValue(lift(TestEntityEmb(Emb("s", 0), 1L, None))).returningGenerated(_.emb.i)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity (s,l,o) VALUES (?, ?, ?) RETURNING i"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "two embedded properties" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.insertValue(lift(TestEntityEmb(Emb("s", 0), 1L, None))).returningGenerated(r => (r.emb.i, r.emb.s))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity (l,o) VALUES (?, ?) RETURNING i, s"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "query with filter using id - id should be excluded" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb
              .insertValue(lift(TestEntityEmb(Emb("s", 0), 1L, None)))
              .returningGenerated(r => (query[Dummy].filter(d => d.i == r.emb.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,l,o) VALUES (?, ?, ?) RETURNING (SELECT MAX(d.*) FROM Dummy d WHERE d.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
      }
      "with returning clause - query" - {
        case class Dummy(i: Int)
        case class DummyS(s: String)
        case class Dummy2(i: Int, s: String)

        "simple not using id - id not excluded" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returningGenerated(r => (query[Dummy].map(d => d.i).value))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING (SELECT d.i FROM Dummy d)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id excluded - map" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returningGenerated(r => (r.i, query[Dummy].map(d => d.i).value))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,l,o,b) VALUES (?, ?, ?, ?) RETURNING r.i, (SELECT d.i FROM Dummy d)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id excluded - filter" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returningGenerated(r => (query[Dummy].filter(d => d.i == r.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,l,o,b) VALUES (?, ?, ?, ?) RETURNING (SELECT MAX(d.*) FROM Dummy d WHERE d.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id excluded - filter + max" in testContext.withDialect(MirrorSqlDialectWithReturnClause) {
          ctx =>
            import ctx._
            val q = quote {
              qr1
                .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
                .returningGenerated(r => (query[Dummy].filter(d => d.i == r.i).max))
            }
            val mirror = ctx.run(q)
            mirror.string mustEqual "INSERT INTO TestEntity AS r (s,l,o,b) VALUES (?, ?, ?, ?) RETURNING (SELECT MAX(d.*) FROM Dummy d WHERE d.i = r.i)"
            mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable, default values - id not excluded - map" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returningGenerated(r => (r.s, r.i, r.l, r.o, r.b, query[Dummy].map(r => r.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r DEFAULT VALUES RETURNING r.s, r.i, r.l, r.o, r.b, (SELECT MAX(r1.i) FROM Dummy r1)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id not excluded - filter + lifting" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val value = 123
          val q = quote {
            qr1
              .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returningGenerated(r => (query[Dummy].filter(r => r.i == lift(value)).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING (SELECT MAX(r1.*) FROM Dummy r1 WHERE r1.i = ?)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        // The `id` column should not be excluded from the insert when it is not actually in a query inside the returning clause.
        "shadow variable - id not excluded - map" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returningGenerated(r => (query[Dummy].map(r => r.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING (SELECT MAX(r1.i) FROM Dummy r1)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id not excluded - join" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returningGenerated(r =>
                (
                  query[Dummy].join(query[Dummy]).on((r, x) => r.i == x.i).map(_._1.i).max
                )
              )
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING (SELECT MAX(r1.i) FROM Dummy r1 INNER JOIN Dummy x ON r1.i = x.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id not excluded - flatMap" in testContext.withDialect(MirrorSqlDialectWithReturnClause) {
          ctx =>
            import ctx._
            val q = quote {
              qr1
                .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
                .returningGenerated(r =>
                  (
                    query[Dummy].flatMap(r => query[Dummy].filter(s => r.i == s.i)).map(_.i).max
                  )
                )
            }
            val mirror = ctx.run(q)
            mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING (SELECT MAX(s.i) FROM Dummy r1, Dummy s WHERE r1.i = s.i)"
            mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id not excluded - concatMap" in testContext.withDialect(MirrorSqlDialectWithReturnClause) {
          ctx =>
            import ctx._
            val q = quote {
              qr1
                .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
                .returningGenerated(r =>
                  (
                    query[DummyS].concatMap(r => r.s.split(" ")).max
                  )
                )
            }
            val mirror = ctx.run(q)
            mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING (SELECT UNNEST(MAX(SPLIT(r1.s, ' '))) FROM DummyS r1)"
            mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id not excluded - groupBy" in testContext.withDialect(MirrorSqlDialectWithReturnClause) {
          ctx =>
            import ctx._
            val q = quote {
              qr1
                .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
                .returningGenerated(r => query[Dummy].groupBy(r => r.i).map(_._2.map(_.i).min).value)
            }
            val mirror = ctx.run(q)
            mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING (SELECT MIN(r1.i) FROM Dummy r1 GROUP BY r1.i)"
            mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id not excluded - flatJoin" in testContext.withDialect(MirrorSqlDialectWithReturnClause) {
          ctx =>
            import ctx._
            val q = quote {
              qr1
                .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
                .returningGenerated(r =>
                  {
                    for {
                      d  <- query[Dummy]
                      d2 <- query[Dummy2].join(r => r.i == d.i)
                    } yield (d.i)
                  }.value
                )
            }
            val mirror = ctx.run(q)
            mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING (SELECT d.i FROM Dummy d INNER JOIN Dummy2 r1 ON r1.i = d.i)"
            mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id not excluded - sortBy" in testContext.withDialect(MirrorSqlDialectWithReturnClause) {
          ctx =>
            import ctx._
            val q = quote {
              qr1
                .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
                .returningGenerated(r => query[Dummy].sortBy(r => r.i).max)
            }
            val mirror = ctx.run(q)
            mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING (SELECT MAX(r1.*) FROM Dummy r1 ORDER BY r1.i ASC NULLS FIRST)"
            mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable in multiple clauses - id not excluded" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returningGenerated(r =>
                (query[Dummy]
                  .filter(r => r.i == r.i /* always true since r overridden! */ )
                  .map(r => r.i)
                  .max)
              )
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING (SELECT MAX(r1.i) FROM Dummy r1 WHERE r1.i = r1.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        // Situation where there are multiple aliases but r.i of the inserted column is actually used
        "shadow variable in one of multiple clauses - id excluded" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returningGenerated(r => (query[Dummy].filter(d => d.i == r.i).map(r => r.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,l,o,b) VALUES (?, ?, ?, ?) RETURNING (SELECT MAX(d.i) FROM Dummy d WHERE d.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        // Same as before just with a second filter clause. This is slightly different since filter clause aliases are not necessarily beta-reduced away.
        "shadow variable in one of multiple clauses - id excluded - two filter" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returningGenerated(r => (query[Dummy].filter(r => r.i == r.i).filter(d => d.i == r.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,l,o,b) VALUES (?, ?, ?, ?) RETURNING (SELECT MAX(r1.*) FROM Dummy r1 WHERE r1.i = r1.i AND r1.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable in one of multiple clauses - id excluded - two filter plus tuple" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returningGenerated(r => (r.i, query[Dummy].filter(r => r.i == r.i).filter(d => d.i == r.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,l,o,b) VALUES (?, ?, ?, ?) RETURNING r.i, (SELECT MAX(r1.*) FROM Dummy r1 WHERE r1.i = r1.i AND r1.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
      }
      "output clause - single" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returningGenerated(_.l)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,o,b) OUTPUT INSERTED.l VALUES (?, ?, ?, ?)"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "output clause - multi" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returningGenerated(r => (r.i, r.l))
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,o,b) OUTPUT INSERTED.i, INSERTED.l VALUES (?, ?, ?)"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "output clause - operation" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returningGenerated(r => (r.i, r.l + 1))
        }
        val mirror = ctx.run(q)

        mirror.string mustEqual "INSERT INTO TestEntity (s,o,b) OUTPUT INSERTED.i, INSERTED.l + 1 VALUES (?, ?, ?)"
      }
      "output clause - record" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returningGenerated(r => r)
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity OUTPUT INSERTED.s, INSERTED.i, INSERTED.l, INSERTED.o, INSERTED.b DEFAULT VALUES"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "output clause - embedded" - {
        case class Dummy(i: Int)

        "embedded property" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.insertValue(lift(TestEntityEmb(Emb("s", 0), 1L, None))).returningGenerated(_.emb.i)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity (s,l,o) OUTPUT INSERTED.i VALUES (?, ?, ?)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "two embedded properties" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.insertValue(lift(TestEntityEmb(Emb("s", 0), 1L, None))).returningGenerated(r => (r.emb.i, r.emb.s))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity (l,o) OUTPUT INSERTED.i, INSERTED.s VALUES (?, ?)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
      }
      "output clause - naming strategy renames INSERTED clause with correct column naming" in testContextSnake
        .withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
          case class Person(firstName: String, age: Int)
          import ctx._
          val q = quote {
            query[Person].insertValue(lift(Person("Joe", 123))).returning(p => p.firstName)
          }

          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO person (first_name,age) OUTPUT INSERTED.first_name VALUES (?, ?)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
      "output clause - should fail on query" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        """import ctx._; quote { qr4.insert(lift(TestEntity4(1L))).returningGenerated(r => query[TestEntity4].filter(t => t.i == r.i)) }""" mustNot compile
      }
    }

    "insert returning" - {
      "multi" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returning(_.l)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o,b) VALUES (?, ?, ?, ?, ?)"
        mirror.returningBehavior mustEqual ReturnColumns(List("l"))
      }
      "multi - should fail on operation" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
        """import ctx._; quote { qr1.insert(lift(TestEntity("s", 0, 1L, None))).returning(r => (r.i, r.l + 1)) }""" mustNot compile
      }
      "no return - should fail on property" in testContext.withDialect(MirrorSqlDialectWithNoReturn) { ctx =>
        """import ctx._; quote { qr1.insert(lift(TestEntity("s", 0, 1L, None))).returning(r => r.i) }""" mustNot compile
      }
      "returning clause - single" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returning(_.l)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING l"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - multi" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returning(r => (r.i, r.l))
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING i, l"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - operation" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returning(r => (r.i, r.l + 1))
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING i, l + 1"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - embedded" - {
        case class Dummy(i: Int)

        "embedded property" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.insertValue(lift(TestEntityEmb(Emb("s", 0), 1L, None))).returning(_.emb.i)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING i"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "two embedded properties" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.insertValue(lift(TestEntityEmb(Emb("s", 0), 1L, None))).returning(r => (r.emb.i, r.emb.s))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING i, s"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "query with filter using id - id should be excluded" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb
              .insertValue(lift(TestEntityEmb(Emb("s", 0), 1L, None)))
              .returning(r => (query[Dummy].filter(d => d.i == r.emb.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING (SELECT MAX(d.*) FROM Dummy d WHERE d.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
      }
      "with returning clause - query" - {
        case class Dummy(i: Int)

        "simple not using id - id not excluded" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returning(r => (query[Dummy].map(d => d.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING (SELECT MAX(d.i) FROM Dummy d)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "simple with id - id would be excluded (if was returningGenerated)" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returning(r => (r.i, query[Dummy].map(d => d.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING r.i, (SELECT MAX(d.i) FROM Dummy d)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "simple with filter using id - id would be excluded (if was returningGenerated)" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returning(r => (query[Dummy].filter(d => d.i == r.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING (SELECT MAX(d.*) FROM Dummy d WHERE d.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id not excluded (same as returningGenerated)" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returning(r => (query[Dummy].map(r => r.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING (SELECT MAX(r1.i) FROM Dummy r1)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable in multiple clauses - id not excluded (same as returningGenerated)" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returning(r =>
                (query[Dummy]
                  .filter(r => r.i == r.i /* always true since r overridden! */ )
                  .map(r => r.i)
                  .max)
              )
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING (SELECT MAX(r1.i) FROM Dummy r1 WHERE r1.i = r1.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable in one of multiple clauses - id excluded" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insertValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returning(r => (query[Dummy].filter(d => d.i == r.i).map(r => r.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) RETURNING (SELECT MAX(d.i) FROM Dummy d WHERE d.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
      }
      "output clause - single" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returning(_.l)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o,b) OUTPUT INSERTED.l VALUES (?, ?, ?, ?, ?)"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "output clause - multi" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returning(r => (r.i, r.l))
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o,b) OUTPUT INSERTED.i, INSERTED.l VALUES (?, ?, ?, ?, ?)"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "output clause - operation" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        import ctx._
        val q      = quote(qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returning(r => (r.i, r.l + 1)))
        val mirror = ctx.run(q)

        mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o,b) OUTPUT INSERTED.i, INSERTED.l + 1 VALUES (?, ?, ?, ?, ?)"
      }
      "output clause - record" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insertValue(lift(TestEntity("s", 0, 1L, None, true))).returning(r => r)
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o,b) OUTPUT INSERTED.s, INSERTED.i, INSERTED.l, INSERTED.o, INSERTED.b VALUES (?, ?, ?, ?, ?)"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "output clause - embedded" - {
        "embedded property" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.insertValue(lift(TestEntityEmb(Emb("s", 0), 1L, None))).returning(_.emb.i)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o) OUTPUT INSERTED.i VALUES (?, ?, ?, ?)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "two embedded properties" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.insertValue(lift(TestEntityEmb(Emb("s", 0), 1L, None))).returning(r => (r.emb.i, r.emb.s))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o) OUTPUT INSERTED.i, INSERTED.s VALUES (?, ?, ?, ?)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
      }
      "output clause - should fail on query" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        """import ctx._; quote { qr4.insert(lift(TestEntity4(1L))).returning(r => query[TestEntity4].filter(t => t.i == r.i)) }""" mustNot compile
      }
    }

    "update returning" - {
      "multi" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
        import ctx._
        val q = quote {
          qr1.updateValue(lift(TestEntity("s", 0, 1L, None, true))).returning(_.l)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "UPDATE TestEntity SET s = ?, i = ?, l = ?, o = ?, b = ?"
        mirror.returningBehavior mustEqual ReturnColumns(List("l"))
      }
      "multi - should fail on operation" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
        """import ctx._; quote { qr1.update(lift(TestEntity("s", 0, 1L, None))).returning(r => (r.i, r.l + 1)) }""" mustNot compile
      }
      "no return - should fail on property" in testContext.withDialect(MirrorSqlDialectWithNoReturn) { ctx =>
        """import ctx._; quote { qr1.update(lift(TestEntity("s", 0, 1L, None))).returning(r => r.i) }""" mustNot compile
      }

      "returning clause - single" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.update(_.s -> "s").returning(_.l)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "UPDATE TestEntity SET s = 's' RETURNING l"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning many clause - single" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.update(_.s -> "s").returningMany(_.l)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "UPDATE TestEntity SET s = 's' RETURNING l"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - multi" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.updateValue(lift(TestEntity("s", 0, 1L, None, true))).returning(r => (r.i, r.l))
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "UPDATE TestEntity SET s = ?, i = ?, l = ?, o = ?, b = ? RETURNING i, l"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - operation" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.updateValue(lift(TestEntity("s", 0, 1L, None, true))).returning(r => (r.i, r.l + 1))
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "UPDATE TestEntity SET s = ?, i = ?, l = ?, o = ?, b = ? RETURNING i, l + 1"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - embedded" - {
        case class Dummy(i: Int)

        "embedded property" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.updateValue(lift(TestEntityEmb(Emb("s", 0), 1L, None))).returning(_.emb.i)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "UPDATE TestEntity SET s = ?, i = ?, l = ?, o = ? RETURNING i"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "two embedded properties" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.updateValue(lift(TestEntityEmb(Emb("s", 0), 1L, None))).returning(r => (r.emb.i, r.emb.s))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "UPDATE TestEntity SET s = ?, i = ?, l = ?, o = ? RETURNING i, s"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "query with filter using id - id should be excluded" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb
              .updateValue(lift(TestEntityEmb(Emb("s", 0), 1L, None)))
              .returning(r => query[Dummy].filter(d => d.i == r.emb.i).max)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "UPDATE TestEntity AS r SET s = ?, i = ?, l = ?, o = ? RETURNING (SELECT MAX(d.*) FROM Dummy d WHERE d.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
      }
      "with returning clause - query" - {
        case class Dummy(i: Int)

        "simple not using id - id not excluded" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .updateValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returning(r => query[Dummy].map(d => d.i).max)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "UPDATE TestEntity AS r SET s = ?, i = ?, l = ?, o = ?, b = ? RETURNING (SELECT MAX(d.i) FROM Dummy d)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "simple with id - id would be excluded (if was returningGenerated)" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .updateValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returning(r => (r.i, query[Dummy].map(d => d.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "UPDATE TestEntity AS r SET s = ?, i = ?, l = ?, o = ?, b = ? RETURNING r.i, (SELECT MAX(d.i) FROM Dummy d)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "simple with filter using id - id would be excluded (if was returningGenerated)" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .updateValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returning(r => query[Dummy].filter(d => d.i == r.i).max)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "UPDATE TestEntity AS r SET s = ?, i = ?, l = ?, o = ?, b = ? RETURNING (SELECT MAX(d.*) FROM Dummy d WHERE d.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id not excluded (same as returningGenerated)" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .updateValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returning(r => query[Dummy].map(r => r.i).max)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "UPDATE TestEntity AS r SET s = ?, i = ?, l = ?, o = ?, b = ? RETURNING (SELECT MAX(r1.i) FROM Dummy r1)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable in multiple clauses - id not excluded (same as returningGenerated)" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .updateValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returning(r =>
                (query[Dummy]
                  .filter(r => r.i == r.i /* always true since r overridden! */ )
                  .map(r => r.i)
                  .max)
              )
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "UPDATE TestEntity AS r SET s = ?, i = ?, l = ?, o = ?, b = ? RETURNING (SELECT MAX(r1.i) FROM Dummy r1 WHERE r1.i = r1.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable in one of multiple clauses - id excluded" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .updateValue(lift(TestEntity("s", 0, 1L, None, true)))
              .returning(r => query[Dummy].filter(d => d.i == r.i).map(r => r.i).max)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "UPDATE TestEntity AS r SET s = ?, i = ?, l = ?, o = ?, b = ? RETURNING (SELECT MAX(d.i) FROM Dummy d WHERE d.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
      }
      "output clause - single" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.updateValue(lift(TestEntity("s", 0, 1L, None, true))).returning(_.l)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "UPDATE TestEntity SET s = ?, i = ?, l = ?, o = ?, b = ? OUTPUT INSERTED.l"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "output clause - multi" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.updateValue(lift(TestEntity("s", 0, 1L, None, true))).returning(r => (r.i, r.l))
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "UPDATE TestEntity SET s = ?, i = ?, l = ?, o = ?, b = ? OUTPUT INSERTED.i, INSERTED.l"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "output clause - operation" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        import ctx._
        val q      = quote(qr1.updateValue(lift(TestEntity("s", 0, 1L, None, true))).returning(r => (r.i, r.l + 1)))
        val mirror = ctx.run(q)

        mirror.string mustEqual "UPDATE TestEntity SET s = ?, i = ?, l = ?, o = ?, b = ? OUTPUT INSERTED.i, INSERTED.l + 1"
      }
      "output clause - record" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.updateValue(lift(TestEntity("s", 0, 1L, None, true))).returning(r => r)
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "UPDATE TestEntity SET s = ?, i = ?, l = ?, o = ?, b = ? OUTPUT INSERTED.s, INSERTED.i, INSERTED.l, INSERTED.o, INSERTED.b"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "output clause - embedded" - {
        "embedded property" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.updateValue(lift(TestEntityEmb(Emb("s", 0), 1L, None))).returning(_.emb.i)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "UPDATE TestEntity SET s = ?, i = ?, l = ?, o = ? OUTPUT INSERTED.i"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "two embedded properties" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.updateValue(lift(TestEntityEmb(Emb("s", 0), 1L, None))).returning(r => (r.emb.i, r.emb.s))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "UPDATE TestEntity SET s = ?, i = ?, l = ?, o = ? OUTPUT INSERTED.i, INSERTED.s"
          mirror.returningBehavior mustEqual ReturnRecord
        }
      }
      "output clause - should fail on query" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        """import ctx._; quote { qr4.update(lift(TestEntity4(1L))).returning(r => query[TestEntity4].filter(t => t.i == r.i)) }""" mustNot compile
      }
    }

    "delete returning" - {
      "multi" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
        import ctx._
        val q = quote {
          qr1.delete.returning(_.l)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "DELETE FROM TestEntity"
        mirror.returningBehavior mustEqual ReturnColumns(List("l"))
      }
      "multi - should fail on operation" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
        """import ctx._; quote { qr1.delete.returning(r => (r.i, r.l + 1)) }""" mustNot compile
      }
      "no return - should fail on property" in testContext.withDialect(MirrorSqlDialectWithNoReturn) { ctx =>
        """import ctx._; quote { qr1.delete.returning(r => r.i) }""" mustNot compile
      }

      "returning clause - single" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.delete.returning(_.l)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "DELETE FROM TestEntity RETURNING l"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning many clause - single" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.delete.returningMany(_.l)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "DELETE FROM TestEntity RETURNING l"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - multi" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.delete.returning(r => (r.i, r.l))
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "DELETE FROM TestEntity RETURNING i, l"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - operation" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.delete.returning(r => (r.i, r.l + 1))
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "DELETE FROM TestEntity RETURNING i, l + 1"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - embedded" - {
        case class Dummy(i: Int)

        "embedded property" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.delete.returning(_.emb.i)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "DELETE FROM TestEntity RETURNING i"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "two embedded properties" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.delete.returning(r => (r.emb.i, r.emb.s))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "DELETE FROM TestEntity RETURNING i, s"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "query with filter using id - id should be excluded" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.delete.returning(r => query[Dummy].filter(d => d.i == r.emb.i).max)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "DELETE FROM TestEntity AS r RETURNING (SELECT MAX(d.*) FROM Dummy d WHERE d.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
      }
      "with returning clause - query" - {
        case class Dummy(i: Int)

        "simple not using id - id not excluded" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1.delete.returning(r => query[Dummy].map(d => d.i).max)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "DELETE FROM TestEntity AS r RETURNING (SELECT MAX(d.i) FROM Dummy d)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "simple with id - id would be excluded (if was returningGenerated)" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1.delete.returning(r => (r.i, query[Dummy].map(d => d.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "DELETE FROM TestEntity AS r RETURNING r.i, (SELECT MAX(d.i) FROM Dummy d)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "simple with filter using id - id would be excluded (if was returningGenerated)" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1.delete.returning(r => query[Dummy].filter(d => d.i == r.i).max)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "DELETE FROM TestEntity AS r RETURNING (SELECT MAX(d.*) FROM Dummy d WHERE d.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id not excluded (same as returningGenerated)" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1.delete.returning(r => query[Dummy].map(r => r.i).max)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "DELETE FROM TestEntity AS r RETURNING (SELECT MAX(r1.i) FROM Dummy r1)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable in multiple clauses - id not excluded (same as returningGenerated)" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1.delete
              .returning(r =>
                (query[Dummy]
                  .filter(r => r.i == r.i /* always true since r overridden! */ )
                  .map(r => r.i)
                  .max)
              )
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "DELETE FROM TestEntity AS r RETURNING (SELECT MAX(r1.i) FROM Dummy r1 WHERE r1.i = r1.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable in one of multiple clauses - id excluded" in testContext.withDialect(
          MirrorSqlDialectWithReturnClause
        ) { ctx =>
          import ctx._
          val q = quote {
            qr1.delete
              .returning(r => query[Dummy].filter(d => d.i == r.i).map(r => r.i).max)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "DELETE FROM TestEntity AS r RETURNING (SELECT MAX(d.i) FROM Dummy d WHERE d.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
      }
      "output clause - single" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.delete.returning(_.l)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "DELETE FROM TestEntity OUTPUT DELETED.l"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "output clause - multi" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.delete.returning(r => (r.i, r.l))
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "DELETE FROM TestEntity OUTPUT DELETED.i, DELETED.l"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "output clause - operation" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        import ctx._
        val q      = quote(qr1.delete.returning(r => (r.i, r.l + 1)))
        val mirror = ctx.run(q)

        mirror.string mustEqual "DELETE FROM TestEntity OUTPUT DELETED.i, DELETED.l + 1"
      }
      "output clause - record" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.delete.returning(r => r)
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "DELETE FROM TestEntity OUTPUT DELETED.s, DELETED.i, DELETED.l, DELETED.o, DELETED.b"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "output clause - embedded" - {
        "embedded property" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.delete.returning(_.emb.i)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "DELETE FROM TestEntity OUTPUT DELETED.i"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "two embedded properties" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.delete.returning(r => (r.emb.i, r.emb.s))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "DELETE FROM TestEntity OUTPUT DELETED.i, DELETED.s"
          mirror.returningBehavior mustEqual ReturnRecord
        }
      }
      "output clause - should fail on query" in testContext.withDialect(MirrorSqlDialectWithOutputClause) { ctx =>
        """import ctx._; quote { qr4.delete.returning(r => query[TestEntity4].filter(t => t.i == r.i)) }""" mustNot compile
      }
    }
  }
  "apply naming strategy to returning action" in testContext.withNaming(SnakeCase) { ctx =>
    import ctx._
    case class TestEntity4(intId: Int, textCol: String)
    val q = quote {
      query[TestEntity4].insertValue(lift(TestEntity4(1, "s"))).returningGenerated(_.intId)
    }
    val mirror = ctx.run(q)
    mirror.string mustEqual "INSERT INTO test_entity4 (text_col) VALUES (?)"
    mirror.returningBehavior mustEqual ReturnColumns(List("int_id"))
  }
}
