# Query compilation benchmark

JMH measures query compilation, including Scala compilation, Quill expansion,
and JDBC decoder derivation. Each invocation recompiles a query from source
using a shared compiler and precompiled schema classes. Source preparation and
output cleanup happen outside the measurement.

## Compare a change

From the repository root, record the current code as your baseline:

```sh
sbt benchBaseline
```

Apply your Quill changes, then run:

```sh
sbt benchCompare
```

The comparison prints each query's baseline time, current time, and percentage
change. Negative means faster. Small differences may be measurement noise.
Both runs use 100 warmups, 15 measurements, and 3 JVM forks per query.
Keep the query fixtures, Scala compiler and machine conditions unchanged.

To focus on specific queries, use `sbt 'benchBaseline OrderDetails,Revenue'`;
`benchCompare` automatically runs the same selection. Results are saved in
`quill-benchmarks/target/{baseline,current}.json`; another `benchBaseline` replaces the
reference, and `clean` removes saved results.

To check compilation without running benchmarks: `sbt 'quill-benchmarks/test'`.

## Workloads

The fixtures model an order-management application using PostgreSQL, with mapped
opaque IDs, enums, optional fields, timestamps, decimals, and wide or nested results.

| Fixture | Coverage |
| --- | --- |
| `Lookup` | Primary-key lookup with a mapped customer ID |
| `Search` | Reused inline query, lifted IN collection, optional LIKE filter, pagination |
| `OrderDetails` | Five joins, dependent optional products, latest status via DISTINCT ON |
| `Revenue` | Window function, conditional projection, grouping, mapped enum array, aggregate filter |
| `Exists` | Parameterized inline predicate with correlated EXISTS and NOT EXISTS |
| `Union` | UNION ALL, DISTINCT, and sorting |
| `Insert` | Whole-record insert with a generated ID returned |
| `BatchInsert` | Lifted collection of records and batch insert |
| `Update` | Optimistic version check, nullable timestamp update, whole row returned |
| `BatchUpdate` | Composite-key batch updates with arithmetic assignments |
| `Delete` | Conditional deletion with null checks and correlated subqueries |
| `Upsert` | Wide-record batch upsert with twelve conflict-update assignments |
