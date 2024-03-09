package io.github.tt432

import scala.annotation.targetName
import scala.util.{Failure, Success, Try}

/**
 * @author TT432
 */

class ObjectCodec[F, R](val map: Map[String, FieldCodec[?, R]], val f: F) extends Codec[R] {
  override def encode[T](dataOps: DataOps[T], value: R): T =
    dataOps.createMap(map.map((k, v) => (k, v.codec.encode(dataOps, v.getter(value)))))

  override def decode[T](dataOps: DataOps[T], source: T): Try[R] =
    Try[R] {
      val dataMap = dataOps.getMap(source)
      var v: Any = f.asInstanceOf[Any => Any]

      for (data <- map.map((k, v) => v.decode(dataOps, dataMap(k))).toList.reverseIterator) {
        data match
          case Success(succ) => v = v.asInstanceOf[Any => Any](succ)
          case Failure(exception) => throw exception
      }

      v.asInstanceOf[R]
    }
}

case class FieldCodec[V, R](name: String, codec: Codec[V], getter: R => V) extends Codec[V] {
  override def decode[T](dataOps: DataOps[T], source: T): Try[V] = codec.decode(dataOps, source)

  override def encode[T](dataOps: DataOps[T], value: V): T = codec.encode(dataOps, value)
}

sealed trait CodecBuilder[RealType, Result] {
  val params: List[FieldCodec[?, Result]]

  def apply(f: RealType): Codec[Result]
}

object CodecBuilder {
  def result[R]: CodecBuilder[R, R] = new CodecBuilder[R, R] {
    override val params: List[FieldCodec[?, R]] = List()

    override def apply(f: R): Codec[R] =
      throw UnsupportedOperationException("can't build with result only")
  }

  implicit class AddOps[A, R](val acc: CodecBuilder[A, R]) {
    @targetName("and")
    def &:[V](x: FieldCodec[V, R]): CodecBuilder[V => A, R] = new CodecBuilder[V => A, R] {
      override val params: List[FieldCodec[?, R]] = acc.params :+ x

      override def apply(f: V => A): Codec[R] = ObjectCodec(params.map(s => (s.name, s)).toMap, f)
    }
  }
}