#!/usr/bin/env bash

set -e

echo "### Bringing Up sqlserver and oracle Images ###"
time docker-compose up -d sqlserver oracle
echo "### DONE Bringing Up sqlserver and oracle Images ###"

# import setup functions
echo "### Sourcing DB Scripts ###"
. build/setup_db_scripts.sh

# run setup scripts for local databases
echo "### Running Setup for sqlite, mysql, postgres ###"
time setup_sqlite $SQLITE_SCRIPT 127.0.0.1
time setup_mysql $MYSQL_SCRIPT 127.0.0.1
time setup_postgres $POSTGRES_SCRIPT 127.0.0.1

echo "### Running Setup for sqlserver ###"
# setup sqlserver in docker
send_script sqlserver $SQL_SERVER_SCRIPT sqlserver-schema.sql
send_script sqlserver ./build/setup_db_scripts.sh setup_db_scripts.sh
time docker-compose exec -T sqlserver bash -c ". setup_db_scripts.sh && setup_sqlserver sqlserver-schema.sql 127.0.0.1"

echo "### Starting to Wait for Oracle ###"
while ! nc -z 127.0.0.1 11521; do
    echo "Waiting for Oracle"
    sleep 2;
done;
sleep 2;

echo "Oracle Setup Complete"

echo "Databases are ready!"