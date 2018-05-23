# How to Contribute

Instructions on how to contribute to Quill project.

## Building the project using Docker

The only dependency you need to build Quill locally is [Docker](https://www.docker.com/).
Instructions on how to install Docker can be found [here](https://docs.docker.com/engine/installation/).

If you are running Linux, you should also install Docker Compose separately, as described
[here](https://docs.docker.com/compose/install/).

After installing Docker and Docker Compose you have to setup databases:

```bash
docker-compose run --rm setup
```

After that you are ready to build and test the project. The following setp describes how to test the project with
sbt built within docker image. If you would like to use your local sbt,
please visit [Building locally](#building-locally-using-docker-only-for-databases).

To build and test the project:

```bash
docker-compose run --rm sbt sbt test
```

## Building Scala.js targets

The Scala.js targets are disabled by default, use `sbt "project quill-with-js"` to enable them.
The CI build also sets this `project quill-with-js` to force the Scala.js compilation.

## Changing database schema

If any file that creates a database schema was changed then you have to setup the databases again:

```bash
docker-compose down && docker-compose run --rm setup
```

## Changing docker configuration

If `build/Dockerfile-sbt`, `build/Dockerfile-setup`, `docker-compose.yml` or any file used by them was changed then you have to rebuild docker images and to setup the databases again:

```bash
docker-compose down && docker-compose build && docker-compose run --rm setup
```

## Tests

### Running tests

Run all tests:
```bash
docker-compose run --rm sbt sbt test
```

Run specific test:
```bash
docker-compose run --rm sbt sbt "test-only io.getquill.context.sql.SqlQuerySpec"
```

Run all tests in specific sub-project:
```bash
docker-compose run --rm sbt sbt "project quill-async" test
```

Run specific test in specific sub-project:
```bash
docker-compose run --rm sbt sbt "project quill-sqlJVM" "test-only io.getquill.context.sql.SqlQuerySpec"
```

### Debugging tests
1. Run sbt in interactive mode with docker container ports mapped to the host: 
```bash
docker-compose run --service-ports --rm sbt
```

2. Attach debugger to port 15005 of your docker host. In IntelliJ IDEA you should create Remote Run/Debug Configuration, 
change it port to 15005.
3. In sbt command line run tests with `test` or test specific spec by passing full name to `test-only`:
```bash
> test-only io.getquill.context.sql.SqlQuerySpec
```

## Pull Request

In order to contribute to the project, just do as follows:

1. Fork the project
2. Build it locally
3. Code
4. Compile (file will be formatted)
5. Run the tests through `docker-compose run sbt sbt test`
6. If you made changes in *.md files, run `docker-compose run sbt sbt tut` to validate them
7. If everything is ok, commit and push to your fork
8. Create a Pull Request, we'll be glad to review it

## File Formatting 

[Scalariform](http://mdr.github.io/scalariform/) is used as file formatting tool in this project.
Every time you compile the project in sbt, file formatting will be triggered.

## Building locally using Docker only for databases

To restart your database service with database ports exposed to your host machine run:

```bash
docker-compose down && docker-compose run --rm --service-ports setup
```

After that we need to set some environment variables in order to run `sbt` locally.

```bash
export CASSANDRA_HOST=127.0.0.1
export CASSANDRA_PORT=19042
export MYSQL_HOST=127.0.0.1
export MYSQL_PORT=13306
export POSTGRES_HOST=127.0.0.1
export POSTGRES_PORT=15432
export SQL_SERVER_HOST=127.0.0.1
export SQL_SERVER_PORT=11433
export ORIENTDB_HOST=127.0.0.1
export ORIENTDB_PORT=12424
```

Where `127.0.0.1` is address of local docker.
If you have non-local docker change it depending on your settings.

Finally, you can use `sbt` locally.
