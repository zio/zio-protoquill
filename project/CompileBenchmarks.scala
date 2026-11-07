import sbt._
import sbt.Keys._
import pl.project13.scala.sbt.JmhPlugin.autoImport.Jmh
import sjsonnew.shaded.scalajson.ast._
import sjsonnew.support.scalajson.unsafe.Parser

object CompileBenchmarks {
  val commands = Seq(
    Command.args("benchBaseline", "[query names]")((state, queries) =>
      new BenchmarkRun(state).recordBaseline(queries.flatMap(_.split(",")))),
    Command.command("benchCompare")(state => new BenchmarkRun(state).compareBaseline())
  )

  private case class Result(query: String, score: Double, settings: Map[String, JValue])
  private val comparedSettings = Seq(
    "benchmark", "mode", "threads", "forks", "warmupIterations", "warmupTime", "warmupBatchSize",
    "measurementIterations", "measurementTime", "measurementBatchSize", "jmhVersion", "jdkVersion",
    "vmName", "vmVersion", "jvmArgs"
  )

  private def check(condition: Boolean, message: => String): Unit =
    if (!condition) throw new MessageOnlyException(message)

  private def readResults(file: File): Vector[Result] = {
    val JArray(rows) = Parser.parseFromString(IO.read(file)).get.toStandard
    val results = rows.map { json =>
      val JObject(row) = json
      val JObject(params) = row("params")
      val JString(query) = params("query")
      val JObject(metric) = row("primaryMetric")
      val JNumber(value) = metric("score")
      val score = value.toDouble
      check(metric("scoreUnit") == JString("ms/op") && java.lang.Double.isFinite(score) && score > 0,
        s"Invalid result for $query in $file")
      Result(query, score, comparedSettings.map(key => key -> row(key)).toMap)
    }
    check(results.nonEmpty && results.map(_.query).distinct.size == results.size, s"Empty or duplicate results in $file")
    results
  }

  private class BenchmarkRun(state: State) {
    private val extracted = Project.extract(state)
    private val project = LocalProject("quill-benchmarks")
    private val directory = extracted.get(project / target)
    private val baseline = directory / "baseline.json"
    private val current = directory / "current.json"
    private val log = state.log

    def recordBaseline(queries: Seq[String]): State = {
      val (next, _) = run(queries)
      IO.move(current, baseline)
      log.info(s"Baseline saved: $baseline")
      log.info("Apply your Quill changes, then run sbt benchCompare")
      next
    }

    def compareBaseline(): State = {
      check(baseline.isFile, "Record a baseline first: sbt benchBaseline")
      val before = readResults(baseline)
      val (next, after) = run(before.map(_.query))
      val byQuery = after.map(result => result.query -> result).toMap
      val pairs = before.map(result => result -> byQuery(result.query))
      pairs.foreach { case (old, fresh) =>
        val changed = comparedSettings.filter(key => old.settings(key) != fresh.settings(key))
        check(changed.isEmpty, s"Incompatible runs for ${old.query}: ${changed.mkString(", ")}")
      }
      log.info(f"${"Query"}%-18s ${"Baseline ms"}%12s ${"Current ms"}%12s ${"Change"}%9s")
      pairs.foreach { case (old, fresh) =>
        val change = (fresh.score / old.score - 1) * 100
        log.info(f"${old.query}%-18s ${old.score}%12.2f ${fresh.score}%12.2f $change%+8.1f%%")
      }
      log.info("Negative change means faster. Small differences may be measurement noise.")
      log.info(s"Results: $current")
      next
    }

    private def run(queries: Seq[String]): (State, Vector[Result]) = {
      val resources = extracted.get(project / Compile / resourceDirectory)
      val available = (resources / "queries" * "*.scala").get.map(_.base).toSet
      check(queries.forall(available), s"Unknown queries: ${(queries.toSet -- available).mkString(", ")}")
      IO.delete(current)
      val selection = if (queries.isEmpty) "" else " -p query=" + queries.mkString(",")
      val arguments = s""" -foe true -rf json -rff "${current.getAbsolutePath}" .*CompileBenchmark.*$selection"""
      val (next, _) = extracted.runInputTask(project / Jmh / Keys.run, arguments, state)
      val results = readResults(current)
      val expected = if (queries.isEmpty) available else queries.toSet
      check(results.map(_.query).toSet == expected, "Incomplete benchmark results")
      (next, results)
    }
  }
}
