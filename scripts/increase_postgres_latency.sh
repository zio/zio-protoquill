#!/bin/bash

# Make the Postgres DB image have 100ms latency for all requests.
# This gives a more realistic understanding of batch-query performance
# since in most corporate environments DBs are on a separate server
# potentially multiple network-hops away.
docker exec protoquill_postgres_1 tc qdisc add dev eth0 root netem delay 50ms