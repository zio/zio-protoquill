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
