package zio.quill.context.cassandra.zio

import _root_.zio.quill.util.LoadConfig
import _root_.zio.quill.{CassandraContextConfig, CassandraZioSession, Spec}
import _root_.zio.{Runtime, Tag, Unsafe, ZEnvironment, ZIO, ZLayer}
import _root_.zio.stream.{ZSink, ZStream}
import _root_.zio.quill.*

object ZioCassandraSpec {
  def runLayerUnsafe[T: Tag](layer: ZLayer[Any, Throwable, T]): T =
    zio.Unsafe.unsafe { implicit unsafe =>
      zio.Runtime.default.unsafe.run(zio.Scope.global.extend(layer.build)).getOrThrow()
    }.get
}

trait ZioCassandraSpec extends Spec {

  var pool: CassandraZioSession = _

  override def beforeAll() = {
    super.beforeAll()
    val config = CassandraContextConfig(LoadConfig("testStreamDB"))
    pool = CassandraZioSession(config.session, config.preparedStatementCacheSize)
    ()
  }

  override def afterAll(): Unit = {
    pool.close()
  }

  def accumulate[T](stream: ZStream[CassandraZioSession, Throwable, T]): ZIO[CassandraZioSession, Throwable, List[T]] =
    stream.run(ZSink.collectAll).map(_.toList)

  def result[T](stream: ZStream[CassandraZioSession, Throwable, T]): List[T] =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(stream.run(ZSink.collectAll).map(_.toList).provideEnvironment(ZEnvironment(pool))).getOrThrow()
    }

  def result[T](qzio: ZIO[CassandraZioSession, Throwable, T]): T =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(qzio.provideEnvironment(ZEnvironment(pool))).getOrThrow()
    }

  implicit class ZStreamTestExt[T](stream: ZStream[CassandraZioSession, Throwable, T]) {
    def runSyncUnsafe() = result[T](stream)
  }

  implicit class ZioTestExt[T](qzio: ZIO[CassandraZioSession, Throwable, T]) {
    def runSyncUnsafe() = result[T](qzio)
  }
}
