#!/usr/bin/env bash

export SQLITE_SCRIPT=quill-jdbc/src/test/resources/sql/sqlite-schema.sql
export MYSQL_SCRIPT=quill-sql/src/test/sql/mysql-schema.sql
export POSTGRES_SCRIPT=quill-sql/src/test/sql/postgres-schema.sql
export SQL_SERVER_SCRIPT=quill-sql/src/test/sql/sqlserver-schema.sql
export CASSANDRA_SCRIPT=quill-cassandra/src/test/cql/cassandra-schema.cql


function get_host() {
    if [ -z "$1" ]; then
        echo "127.0.0.1"
    else
        echo "$1"
    fi
}
# usage: setup_x <script>

function setup_sqlite() {
    DB_FILE=quill-jdbc/quill_test.db
    rm -f $DB_FILE
    sqlite3 $DB_FILE < $1
    chmod a+rw $DB_FILE
    echo "Sqlite ready!"
}

function setup_mysql() {
    connection=$2
    if [[ "$2" == "mysql" ]]; then
       conn="mysql -proot"
       hacks="mysql -h mysql -u root -proot -e \"ALTER USER 'root'@'%' IDENTIFIED BY ''\""
    fi

    echo "Waiting for MySql"
    until mysql -h $connection -u root -e "select 1" &> /dev/null; do
        sleep 5;
    done
    echo "Connected to MySql"

    eval $hacks
    mysql -h $2 -u root -e "CREATE DATABASE quill_test;"
    mysql -h $2 -u root quill_test < $1
    mysql -h $2 -u root -e "CREATE USER 'finagle'@'%' IDENTIFIED BY 'finagle';"
    mysql -h $2 -u root -e "GRANT ALL PRIVILEGES ON * . * TO 'finagle'@'%';"
    mysql -h $2 -u root -e "FLUSH PRIVILEGES;"
}

function setup_postgres() {
    host=$(get_host $2)
    echo "Waiting for Postgres"
    until psql -h $2 -U postgres -c "select 1" &> /dev/null; do
        sleep 5;
    done
    echo "Connected to Postgres"

    psql -h $2 -U postgres -c "CREATE DATABASE quill_test"
    psql -h $2 -U postgres -d quill_test -a -q -f $1
}

function setup_cassandra() {
    host=$(get_host $2)
    echo "Waiting for Cassandra"
    until cqlsh $2 -e "describe cluster" &> /dev/null; do
        sleep 5;
    done
    echo "Connected to Cassandra"

    cqlsh $2 -f $1
}

function setup_sqlserver() {
    host=$(get_host $2)
    echo "Waiting for SqlServer"
    until /opt/mssql-tools/bin/sqlcmd -S $2 -U SA -P "QuillRocks!" -Q "select 1" &> /dev/null; do
        sleep 5;
    done
    echo "Connected to SqlServer"

    /opt/mssql-tools/bin/sqlcmd -S $2 -U SA -P "QuillRocks!" -Q "CREATE DATABASE quill_test"
    /opt/mssql-tools/bin/sqlcmd -S $2 -U SA -P "QuillRocks!" -d quill_test -i $1
}

export -f setup_sqlite
export -f setup_mysql
export -f setup_postgres
export -f setup_cassandra
export -f setup_sqlserver