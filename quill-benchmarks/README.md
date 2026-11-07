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

To focus on specific queries, use `sbt 'benchBaseline Lookup'`;
`benchCompare` automatically runs the same selection. Results are saved in
`quill-benchmarks/target/{baseline,current}.json`; another `benchBaseline` replaces the
reference, and `clean` removes saved results.

To check compilation without running benchmarks: `sbt 'quill-benchmarks/test'`.

## Workload

The `Lookup` fixture performs a primary-key lookup with a mapped customer ID.
