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
* Allow `infix"..."` without `.as[...]` in some needed cases.

# 3.7.1.Beta1.1

* Aggressively serialize quats via Kryo. This seems to significantly improve performance.
* Introduce `context.prepare` command.

# 3.7.1.Beta1.0

* Initial release
