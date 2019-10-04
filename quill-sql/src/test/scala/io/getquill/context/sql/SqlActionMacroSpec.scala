package io.getquill.context.sql

import io.getquill._
import io.getquill.ReturnAction.{ ReturnColumns, ReturnRecord }
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
          "UPDATE TestEntity SET s = 's' WHERE s IS NULL"
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
          "DELETE FROM TestEntity WHERE s IS NULL"
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

    "returning generated" - {
      "single with returning generated" in {
        val q = quote {
          qr1.insert(lift(TestEntity("s", 0, 1L, None))).returningGenerated(_.l)
        }

        val mirror = testContext.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,o) VALUES (?, ?, ?)"
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
        """testContext.run(qr1.insert(lift(TestEntity("s", 0, 1L, None))).returningGenerated(r => r))""" mustNot compile
      }
      "multi" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
        import ctx._
        val q = quote {
          qr1.insert(lift(TestEntity("s", 0, 1L, None))).returningGenerated(_.l)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,o) VALUES (?, ?, ?)"
        mirror.returningBehavior mustEqual ReturnColumns(List("l"))
      }
      "multi with record type returning" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
        import ctx._
        val q = quote {
          qr1.insert(lift(TestEntity("s", 0, 1L, None))).returning(r => r)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o) VALUES (?, ?, ?, ?)"
        mirror.returningBehavior mustEqual ReturnColumns(List("s", "i", "l", "o"))
      }
      "multi with record type returning generated should exclude all" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
        import ctx._
        val q = quote {
          qr1.insert(lift(TestEntity("s", 0, 1L, None))).returningGenerated(r => r)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity DEFAULT VALUES"
        mirror.returningBehavior mustEqual ReturnColumns(List("s", "i", "l", "o"))
      }
      "multi - should fail on operation" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
        """import ctx._; quote { qr1.insert(lift(TestEntity("s", 0, 1L, None))).returningGenerated(r => (r.i, r.l + 1)) }""" mustNot compile
      }
      "multi - should fail on operation inside case class" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
        case class Foo(one: Int, two: Long)
        """import ctx._; quote { qr1.insert(lift(TestEntity("s", 0, 1L, None))).returningGenerated(r => Foo(r.i, r.l + 1)) }""" mustNot compile
      }
      "no return - should fail on property" in testContext.withDialect(MirrorSqlDialectWithNoReturn) { ctx =>
        """import ctx._; quote { qr1.insert(lift(TestEntity("s", 0, 1L, None))).returningGenerated(r => r.i) }""" mustNot compile
      }
      "returning clause - single" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insert(lift(TestEntity("s", 0, 1L, None))).returningGenerated(_.l)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,o) VALUES (?, ?, ?) RETURNING l"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - multi" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insert(lift(TestEntity("s", 0, 1L, None))).returningGenerated(r => (r.i, r.l))
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,o) VALUES (?, ?) RETURNING i, l"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - operation" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insert(lift(TestEntity("s", 0, 1L, None))).returningGenerated(r => (r.i, r.l + 1))
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,o) VALUES (?, ?) RETURNING i, l + 1"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - record" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insert(lift(TestEntity("s", 0, 1L, None))).returning(r => r)
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING s, i, l, o"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning generated clause - record" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insert(lift(TestEntity("s", 0, 1L, None))).returningGenerated(r => r)
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity DEFAULT VALUES RETURNING s, i, l, o"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - embedded" - {
        case class Dummy(i: Int)

        "embedded property" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.insert(lift(TestEntityEmb(Emb("s", 0), 1L, None))).returningGenerated(_.emb.i)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity (s,l,o) VALUES (?, ?, ?) RETURNING i"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "two embedded properties" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.insert(lift(TestEntityEmb(Emb("s", 0), 1L, None))).returningGenerated(r => (r.emb.i, r.emb.s))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity (l,o) VALUES (?, ?) RETURNING i, s"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "query with filter using id - id should be excluded" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.insert(lift(TestEntityEmb(Emb("s", 0), 1L, None))).returningGenerated(r => (query[Dummy].filter(d => d.i == r.emb.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,l,o) VALUES (?, ?, ?) RETURNING (SELECT MAX(*) FROM Dummy d WHERE d.i = r.i)"
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
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returningGenerated(r => (query[Dummy].map(d => d.i).value))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING (SELECT d.i FROM Dummy d)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id excluded - map" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returningGenerated(r => (r.i, query[Dummy].map(d => d.i).value))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,l,o) VALUES (?, ?, ?) RETURNING r.i, (SELECT d.i FROM Dummy d)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id excluded - filter" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returningGenerated(r => (query[Dummy].filter(d => d.i == r.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,l,o) VALUES (?, ?, ?) RETURNING (SELECT MAX(*) FROM Dummy d WHERE d.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id excluded - filter + max" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returningGenerated(r => (query[Dummy].filter(d => d.i == r.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,l,o) VALUES (?, ?, ?) RETURNING (SELECT MAX(*) FROM Dummy d WHERE d.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable, default values - id not excluded - map" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returningGenerated(r => (r.s, r.i, r.l, r.o, query[Dummy].map(r => r.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r DEFAULT VALUES RETURNING r.s, r.i, r.l, r.o, (SELECT MAX(r1.i) FROM Dummy r1)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id not excluded - filter + lifting" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val value = 123
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returningGenerated(r => (query[Dummy].filter(r => r.i == lift(value)).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING (SELECT MAX(*) FROM Dummy r1 WHERE r1.i = ?)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        // The `id` column should not be excluded from the insert when it is not actually in a query inside the returning clause.
        "shadow variable - id not excluded - map" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returningGenerated(r => (query[Dummy].map(r => r.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING (SELECT MAX(r1.i) FROM Dummy r1)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id not excluded - join" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returningGenerated(r => (
                query[Dummy].join(query[Dummy]).on((r, x) => r.i == x.i).map(_._1.i).max
              ))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING (SELECT MAX(r1.i) FROM Dummy r1 INNER JOIN Dummy x ON r1.i = x.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id not excluded - flatMap" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returningGenerated(r => (
                query[Dummy].flatMap(r => query[Dummy].filter(s => r.i == s.i)).map(_.i).max
              ))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING (SELECT MAX(s.i) FROM Dummy r1, Dummy s WHERE r1.i = s.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id not excluded - concatMap" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returningGenerated(r => (
                query[DummyS].concatMap(r => r.s.split(" ")).max
              ))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING (SELECT UNNEST(MAX(SPLIT(r1.s, ' '))) FROM DummyS r1)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id not excluded - groupBy" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returningGenerated(r =>
                query[Dummy].groupBy(r => r.i).map(_._2.map(_.i).min).value)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING (SELECT MIN(r1.i) FROM Dummy r1 GROUP BY r1.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id not excluded - flatJoin" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returningGenerated(r =>
                {
                  for {
                    d <- query[Dummy]
                    d2 <- query[Dummy2].join(r => r.i == d.i)
                  } yield (d.i)
                }.value)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING (SELECT d.i FROM Dummy d INNER JOIN Dummy2 r1 ON r1.i = d.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id not excluded - sortBy" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returningGenerated(r =>
                query[Dummy].sortBy(r => r.i).max)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING (SELECT MAX(*) FROM Dummy r1 ORDER BY r1.i ASC NULLS FIRST)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable in multiple clauses - id not excluded" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returningGenerated(
                r =>
                  (query[Dummy]
                    .filter(
                      r => r.i == r.i /* always true since r overridden! */
                    )
                    .map(r => r.i)
                    .max)
              )
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING (SELECT MAX(r1.i) FROM Dummy r1 WHERE r1.i = r1.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        // Situation where there are multiple aliases but r.i of the inserted column is actually used
        "shadow variable in one of multiple clauses - id excluded" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returningGenerated(
                r => (query[Dummy].filter(d => d.i == r.i).map(r => r.i).max)
              )
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,l,o) VALUES (?, ?, ?) RETURNING (SELECT MAX(d.i) FROM Dummy d WHERE d.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        // Same as before just with a second filter clause. This is slightly different since filter clause aliases are not necessarily beta-reduced away.
        "shadow variable in one of multiple clauses - id excluded - two filter" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returningGenerated(
                r => (query[Dummy].filter(r => r.i == r.i).filter(d => d.i == r.i).max)
              )
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,l,o) VALUES (?, ?, ?) RETURNING (SELECT MAX(*) FROM Dummy r1 WHERE r1.i = r1.i AND r1.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable in one of multiple clauses - id excluded - two filter plus tuple" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returningGenerated(
                r => (r.i, query[Dummy].filter(r => r.i == r.i).filter(d => d.i == r.i).max)
              )
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,l,o) VALUES (?, ?, ?) RETURNING r.i, (SELECT MAX(*) FROM Dummy r1 WHERE r1.i = r1.i AND r1.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
      }
    }

    "returning" - {
      "multi" in testContext.withDialect(MirrorSqlDialectWithReturnMulti) { ctx =>
        import ctx._
        val q = quote {
          qr1.insert(lift(TestEntity("s", 0, 1L, None))).returning(_.l)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o) VALUES (?, ?, ?, ?)"
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
          qr1.insert(lift(TestEntity("s", 0, 1L, None))).returning(_.l)
        }

        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING l"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - multi" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insert(lift(TestEntity("s", 0, 1L, None))).returning(r => (r.i, r.l))
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING i, l"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - operation" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
        import ctx._
        val q = quote {
          qr1.insert(lift(TestEntity("s", 0, 1L, None))).returning(r => (r.i, r.l + 1))
        }
        val mirror = ctx.run(q)
        mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING i, l + 1"
        mirror.returningBehavior mustEqual ReturnRecord
      }
      "returning clause - embedded" - {
        case class Dummy(i: Int)

        "embedded property" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.insert(lift(TestEntityEmb(Emb("s", 0), 1L, None))).returning(_.emb.i)
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING i"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "two embedded properties" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.insert(lift(TestEntityEmb(Emb("s", 0), 1L, None))).returning(r => (r.emb.i, r.emb.s))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING i, s"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "query with filter using id - id should be excluded" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1Emb.insert(lift(TestEntityEmb(Emb("s", 0), 1L, None))).returning(r => (query[Dummy].filter(d => d.i == r.emb.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING (SELECT MAX(*) FROM Dummy d WHERE d.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
      }
      "with returning clause - query" - {
        case class Dummy(i: Int)

        "simple not using id - id not excluded" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returning(r => (query[Dummy].map(d => d.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING (SELECT MAX(d.i) FROM Dummy d)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "simple with id - id would be excluded (if was returningGenerated)" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returning(r => (r.i, query[Dummy].map(d => d.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING r.i, (SELECT MAX(d.i) FROM Dummy d)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "simple with filter using id - id would be excluded (if was returningGenerated)" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returning(r => (query[Dummy].filter(d => d.i == r.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING (SELECT MAX(*) FROM Dummy d WHERE d.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable - id not excluded (same as returningGenerated)" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returning(r => (query[Dummy].map(r => r.i).max))
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING (SELECT MAX(r1.i) FROM Dummy r1)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable in multiple clauses - id not excluded (same as returningGenerated)" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returning(
                r =>
                  (query[Dummy]
                    .filter(
                      r => r.i == r.i /* always true since r overridden! */
                    )
                    .map(r => r.i)
                    .max)
              )
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING (SELECT MAX(r1.i) FROM Dummy r1 WHERE r1.i = r1.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
        "shadow variable in one of multiple clauses - id excluded" in testContext.withDialect(MirrorSqlDialectWithReturnClause) { ctx =>
          import ctx._
          val q = quote {
            qr1
              .insert(lift(TestEntity("s", 0, 1L, None)))
              .returning(
                r => (query[Dummy].filter(d => d.i == r.i).map(r => r.i).max)
              )
          }
          val mirror = ctx.run(q)
          mirror.string mustEqual "INSERT INTO TestEntity AS r (s,i,l,o) VALUES (?, ?, ?, ?) RETURNING (SELECT MAX(d.i) FROM Dummy d WHERE d.i = r.i)"
          mirror.returningBehavior mustEqual ReturnRecord
        }
      }
    }

  }
  "apply naming strategy to returning action" in testContext.withNaming(SnakeCase) { ctx =>
    import ctx._
    case class TestEntity4(intId: Int, textCol: String)
    val q = quote {
      query[TestEntity4].insert(lift(TestEntity4(1, "s"))).returningGenerated(_.intId)
    }
    val mirror = ctx.run(q)
    mirror.string mustEqual "INSERT INTO test_entity4 (text_col) VALUES (?)"
    mirror.returningBehavior mustEqual ReturnColumns(List("int_id"))
  }
}
