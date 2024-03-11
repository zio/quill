package io.getquill.context.cassandra.pekko

class CaseClassQueryCassandraSpec extends CassandraPekkoSpec {

  import testDB._

  case class Contact(id: Int, firstName: String, lastName: String, age: Int, addressFk: Int, extraInfo: String)
  case class Address(id: Int, street: String, zip: Int, otherExtraInfo: String)

  val peopleInsert =
    quote((p: Contact) => query[Contact].insertValue(p))

  val peopleEntries = List(
    Contact(1, "Alex", "Jones", 60, 2, "foo"),
    Contact(2, "Bert", "James", 55, 3, "bar"),
    Contact(3, "Cora", "Jasper", 33, 3, "baz")
  )

  val addressInsert =
    quote((c: Address) => query[Address].insertValue(c))

  val addressEntries = List(
    Address(1, "123 Fake Street", 11234, "something"),
    Address(2, "456 Old Street", 45678, "something else"),
    Address(3, "789 New Street", 89010, "another thing")
  )

  case class ContactSimplified(firstName: String, lastName: String, age: Int)
  case class AddressableContact(firstName: String, lastName: String, age: Int, street: String, zip: Int)

  val `Ex 1 CaseClass Record Output` = quote {
    query[Contact].map(p => new ContactSimplified(p.firstName, p.lastName, p.age))
  }

  val `Ex 1 CaseClass Record Output expected result` = List(
    ContactSimplified("Alex", "Jones", 60),
    ContactSimplified("Bert", "James", 55),
    ContactSimplified("Cora", "Jasper", 33)
  )

  case class FiltrationObject(idFilter: Int)

  val `Ex 3 Inline Record Usage` = quote {
    val filtrationObject = new FiltrationObject(1)
    query[Contact].filter(p => p.id == filtrationObject.idFilter)
  }

  val `Ex 3 Inline Record Usage expected result` = List(
    new Contact(1, "Alex", "Jones", 60, 2, "foo")
  )

  override def beforeAll = {
    await {
      testDB.run(query[Contact].delete)
    }
    await {
      testDB.run(query[Address].delete)
    }
    await {
      testDB.run(liftQuery(peopleEntries).foreach(p => peopleInsert(p)))
    }
    await {
      testDB.run(liftQuery(addressEntries).foreach(p => addressInsert(p)))
    }
  }

  "Example 1 - Single Case Class Mapping" in {
    await {
      testDB
        .run(`Ex 1 CaseClass Record Output`)
        .map(res => res mustEqual `Ex 1 CaseClass Record Output expected result`)
    }
  }

  "Example 2 - Inline Record as Filter" in {
    await {
      testDB.run(`Ex 3 Inline Record Usage`).map(res => res mustEqual `Ex 3 Inline Record Usage expected result`)
    }
  }
}
