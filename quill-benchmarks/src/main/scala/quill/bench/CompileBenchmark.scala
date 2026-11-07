package quill.bench

import dotty.tools.dotc.{Compiler, Driver}
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.reporting.ConsoleReporter
import org.openjdk.jmh.annotations.*
import java.nio.file.{Files, Path}
import java.util.Comparator
import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized
import scala.util.Using

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 100)
@Measurement(iterations = 15)
@Fork(value = 3, jvmArgsAppend = Array("-Xms1g", "-Xmx4g", "-Dquill.macro.log=false"))
@Threads(1)
class CompileBenchmark {
  @Param(Array("Lookup", "Search", "OrderDetails", "Revenue", "Exists", "Union",
    "Insert", "BatchInsert", "Update", "BatchUpdate", "Delete", "Upsert"))
  var query: String = uninitialized

  // Reuse compiler state; Compiler.newRun resets per-compilation caches.
  private val driver = new Driver {
    private val compiler = new Compiler
    override protected def newCompiler(using Context): Compiler = compiler
    override protected val initCtx: Context = (new ContextBase).initialCtx
  }

  private var directory: Path = uninitialized
  private var arguments: Array[String] = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    val input = getClass.getResourceAsStream(s"/queries/$query.scala")
    require(input != null, s"Unknown query: $query")
    directory = Files.createTempDirectory("quill-compile-")
    val source = directory.resolve(s"$query.scala")
    val output = Files.createDirectory(directory.resolve("classes"))
    Using.resource(input)(Files.copy(_, source))
    arguments = Array(
      "-color:never", "-language:implicitConversions", "-release:17", "-source:3.3", "-Werror",
      "-classpath", System.getProperty("java.class.path"), "-d", output.toString, source.toString
    )
  }

  @Benchmark
  def compile(): Unit = {
    val reporter = driver.process(arguments, new ConsoleReporter)
    require(!reporter.hasErrors, s"Compilation failed: $query")
  }

  @TearDown(Level.Trial)
  def cleanup(): Unit = Using.resource(Files.walk(directory)) { paths =>
    paths.sorted(Comparator.reverseOrder()).forEach(path => Files.delete(path))
  }
}
