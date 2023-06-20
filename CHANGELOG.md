# 4.7.0

- [Update Scala to 3.3.0, sbt to 1.9.0 and HikariCP to 4.0.3](https://github.com/zio/zio-protoquill/pull/276)

# 4.6.0.1

- [Upgrade to latest version of ZIO (2.0.8)](https://github.com/zio/zio-protoquill/pull/242)
- [JDBC DataSource should not need to be closeable](https://github.com/zio/zio-protoquill/pull/205)

# 4.6.0

- [Json/Jsonb Encoding for Postgres. Bump ZIO](https://github.com/zio/zio-protoquill/pull/202)

# 4.5.0

- [Implement Dynamic Query DSL for ProtoQuill](https://github.com/zio/zio-protoquill/pull/198)
- [Fix dynamic Insert/UpdateMeta and better warning message](https://github.com/zio/zio-protoquill/pull/197)
- [Support for structural types in Quoted functions](https://github.com/zio/zio-protoquill/pull/199)
- [Upstream changes. Add JDBC encoders for java.time dates](https://github.com/zio/zio-protoquill/pull/191)

# 4.4.1

- [Fixing Caliban Datatype conversion issues](https://github.com/zio/zio-protoquill/pull/188)
- [Incorporate Quill 4.4.1 change from upstream](https://github.com/zio/zio-protoquill/pull/189)

# 4.4.0

- [Integrating upstream changes for Update VALUES-clause optimization](https://github.com/zio/zio-protoquill/pull/182)

# 4.3.0

- [Implementing multi-row insert for liftQuery(...).foreach](https://github.com/zio/zio-protoquill/pull/172)

# 4.2.0

- [Update zio-idiomatic context](https://github.com/zio/zio-protoquill/pull/164)
- [Implement ZIO-Idiomatic JDBC Context](https://github.com/zio/zio-protoquill/pull/160)
- [Adding idiomatic-zio cassandra context](https://github.com/zio/zio-protoquill/pull/166)
- [Change infix"$content" to sql"$content"](https://github.com/zio/zio-protoquill/pull/165)
- [Upgrade caliban and re-enable quill-caliban](https://github.com/zio/zio-protoquill/pull/161)
- [Remove matrowl and do token splicing at runtime](https://github.com/zio/zio-protoquill/pull/168)
- [Fix for indirect-dynamic](https://github.com/zio/zio-protoquill/pull/157)

#### Migration Notes:
- The `infix` interpolator is now deprecated because in Scala 2, infix is a keyword. Instead of
  `infix"MyUdf(${person.name})"` use `sql"MyUdf(${person.name})"`. For contexts such as Doobie that already
  have an `sql` interpolator. Import `context.compat._` and use the `qsql` interpolator instead.

# 4.1.0-V2

- [Implementing dynamic splices via #${splice} like Scala2-Quill](https://github.com/zio/zio-protoquill/pull/153)

# 4.1.0

- [Integrating groupByMap from Scala2-Quill. Also EnableTrace/DisablePhase](https://github.com/zio/zio-protoquill/pull/151)

# 4.0.0

- [Bump to ZIO 2.0.0 Release Version](https://github.com/zio/zio-protoquill/pull/146)

# 3.19.0

- [Update to Jasync 2. Additional Jasync SSL Configs](https://github.com/zio/zio-protoquill/pull/142)
- [Implement insert/update/delete.returningMany](https://github.com/zio/zio-protoquill/pull/117)

# 3.18.0

- [Correct decoding Option[Product] and fix complex insertValue encoders](https://github.com/zio/zio-protoquill/pull/109)

#### Migration Notes:
- Similar to [2504](https://github.com/zio/zio-quill/pull/2504) in Scala2-Quill, [109](https://github.com/zio/zio-protoquill/pull/109) in ProtoQuill changes the handling of optional-product rows. Whereas before, if any non-optional column of an optional-product row was null, then entre optional-product would be null. Now however, an optional-product will only be null if every column inside is null. For example, before, if a query returning `Person(name:Option(Name(first:String, last:String)), age: Int)` resulted in the row `ResultRow("Joe", null, 123)` before the entity would be decoded into `Person(None, 123)` (i.e. the optional-product `Option[Name]` would decode to `None`).<br>
  Now however, `Option[Name]` only decodes to `None` if every column inside it is null. This means that the `ResultRow("Joe", null, 123)` decodes to `Person(Name("Joe", 0 /*default-placeholder for null*/), 123)`. Only when the both `first` and `last` columns in Name are null i.e. `ResultRow(null, null, 123)` will the result be: `Person(None, 123)`.

# 3.16.5-Beta31

- [Fix Batch Insertion/Update for tuple-based fields](https://github.com/zio/zio-protoquill/pull/103)

# 3.16.5-Beta30

- [Enhance Batch Queries. Support multiple lifts.](https://github.com/zio/zio-protoquill/pull/100)

# 3.16.5-Beta29

- [Integrating Doobie Support](https://github.com/zio/zio-protoquill/pull/98)

# 3.16.4-Beta28

- [Move to top-level. Increment with a dummy version.](https://github.com/zio/zio-protoquill/commit/8597a5f3d45bf5e68fcb8f2df1e2aa510213c5dd)
- [Another IntelliJ (SCL-20185) Workaround](https://github.com/zio/zio-protoquill/commit/6c4f75d9a40804b17dd9182009b9ab8e5d8c0c93)

Note: Changing the version convention due to findings of [SCL-19345](https://youtrack.jetbrains.com/issue/SCL-19345). This should allow correct Scala 3 code identification in IntelliJ. Also moving DSL to top level since Tasty reader seems to process that correctly.

# 3.16.4-Beta2.7

- [Working around semver and multi-inherit limitations in IntelliJ SCL-19345](https://github.com/zio/zio-protoquill/actions/runs/2270134936)

Note: This release is to try to resolve some IntelliJ issues regarding SCL-19345. There are no functionality changes. For this reason I had to change the version convention.

# 3.16.4.Beta2.6

- [Enable context.translate in remaining contexts](https://github.com/zio/zio-protoquill/pull/91)
- [Implement context `translate` methods](https://github.com/zio/zio-protoquill/pull/81)
- [Adding extras module based on Scala2-Quill](https://github.com/zio/zio-protoquill/pull/90)
- [Adding DistinctOn functionality from Scala2-Quill](https://github.com/zio/zio-protoquill/pull/89)
- [Bump to 3.16.4](https://github.com/zio/zio-protoquill/pull/88)

Note: This change should resolve the IntelliJ issue [SCL-20078](https://youtrack.jetbrains.com/issue/SCL-20078)
but as of now still has the [SCL-19345](https://youtrack.jetbrains.com/issue/SCL-19345) issue.

# 3.16.3.Beta2.5

- [Concrete run method signatures in contexts](https://github.com/zio/zio-protoquill/pull/83)
- [Implement context `translate` methods](https://github.com/zio/zio-protoquill/pull/81)

# 3.16.3.Beta2.4

- [Fixing case of filter(lift).insert/updateValue](https://github.com/zio/zio-protoquill/pull/76)

# 3.16.3.Beta2.3

- [Bump engine/util to 3.16.3](https://github.com/zio/zio-protoquill/pull/74)
- [Change AST Serialization to BooPickle](https://github.com/zio/zio-protoquill/pull/72)

# 3.16.1.Beta2.2

- [Bump engine to 3.16.1](https://github.com/zio/zio-protoquill/pull/65)

# 3.16.0.Beta2.1

- [Bump engine version](https://github.com/zio/zio-protoquill/pull/62)
- [Remove deprecated EntityQuery.insert/update APIs](https://github.com/zio/zio-protoquill/pull/61)

* Note: Similar to zio-quill/pull/2413 this change just removes the deprecated
  `EntityQuery.insert(CaseClass)` and `EntityQuery.update(CaseClass)` APIs that have been updated
  to `EntityQuery.insertValue(CaseClass)` and `EntityQuery.updateValue(CaseClass)`,
  and it is the only change in this release. This is needed because of lampepfl/dotty#14043.

# 3.15.0.Beta2.0

- [Bump ZIO to 1.0.12](https://github.com/zio/zio-protoquill/pull/57)
- [Update quill-engine to 3.15.0, update APIs](https://github.com/zio/zio-protoquill/pull/59)
- [Prepare build for upcoming Scala2-Quill dependency and community build integration](https://github.com/zio/zio-protoquill/pull/58)
- [Remove elaboration from SELECT queries](https://github.com/zio/zio-protoquill/pull/56)
- [Introduce implicit hint to fail non-static query build](https://github.com/zio/zio-protoquill/pull/55)
- [ProtoQuill does not support Scala 2 style implicit extensions. Warn about it](https://github.com/zio/zio-protoquill/pull/53)
- [Tail-recursive generic derivation](https://github.com/zio/zio-protoquill/pull/52)
- [Cleanup in various places](https://github.com/zio/zio-protoquill/pull/50)
- [Refactor Parser into a clean ADT](https://github.com/zio/zio-protoquill/pull/48)
- [Fix stack overflow from enums with no encoder](https://github.com/zio/zio-protoquill/pull/47)

# 3.12.0.Beta1.7

* [Fixing Caliban integration & adding quill-caliban to build](https://github.com/zio/zio-protoquill/pull/44)
* [Move ZIO Contexts to latest Scala2-Quill implementations & Cassandra](https://github.com/zio/zio-protoquill/pull/43)
* [use existing transactional context](https://github.com/zio/zio-protoquill/pull/29)

# 3.10.0.Beta1.6

* [Port quill-cassandra](https://github.com/getquill/protoquill/pull/23)
* [Cassandra UDT Encoders/Decoders](https://github.com/getquill/protoquill/pull/25)
* [Implementing Cassandra ZIO Context](https://github.com/getquill/protoquill/pull/26)
* [Refactoring, introducing filterColumns](https://github.com/getquill/protoquill/commit/e070b862075e3beec56ad05c6801608acaa1dd0c)

# 3.10.0.Beta1.5

* [Move to 3.10.0 of base and zio. ProtoContext now in portable.](https://github.com/getquill/protoquill/commit/39c62ab2e6400f9cf4b3d87740900f55fd69ab12)
* [Implement static operator for splicing constants](https://github.com/getquill/protoquill/pull/16)
* [Pass Session to all Encoders/Decoders, implement static operator](https://github.com/getquill/protoquill/pull/18)
* [Lift & Serialize when update-macro state is static. More efficient.](https://github.com/getquill/protoquill/pull/19)
* [Various Fixes for Lifter and Unlifter](https://github.com/getquill/protoquill/pull/20)

This change lines up the zio-jdbc modules with the latest 3.10.0 line. Various fixes are done to Ast lifting as well as the InsertUpdateMacro for increased efficiency. The `static` keyword is introduced.

# 3.7.2.Beta1.4

* [Support for ON CONFLICT in Postgres, MySQL, and SQLite](https://github.com/getquill/protoquill/pull/9)

# 3.7.2.Beta1.3

* [Implement Dynamic Batch Actions](https://github.com/getquill/protoquill/pull/8)

# 3.7.2.Beta1.2

* Kryo-Serialize entire AST. Further improves performance.
* Allow `x -> y` syntax for constructing tuples.
* Allow `sql"..."` without `.as[...]` in some needed cases.

# 3.7.1.Beta1.1

* Aggressively serialize quats via Kryo. This seems to significantly improve performance.
* Introduce `context.prepare` command.

# 3.7.1.Beta1.0

* Initial release
