package io.github.tt432

/**
 * @author TT432
 */

import java.lang.{Boolean, Byte, Double, Float, Long, Short}
import scala.annotation.targetName
import scala.util.{Failure, Success, Try}

trait Codec[V] {
  def decode[T](dataOps: DataOps[T], source: T): Try[V]

  def encode[T](dataOps: DataOps[T], value: V): T
}

trait NumericCodec[V <: Number] extends Codec[V] {
  def from(number: Number): V

  override def decode[T](dataOps: DataOps[T], source: T): Try[V] = Try[V] {
    from(dataOps.getNumber(source))
  }

  override def encode[T](dataOps: DataOps[T], value: V): T = dataOps.createNumber(value)
}

object Codec {
  val long: Codec[Long] = new NumericCodec[Long] {
    override def from(number: Number): Long = number.longValue()
  }

  val int: Codec[Integer] = new NumericCodec[Integer] {
    override def from(number: Number): Integer = number.intValue()
  }

  val short: Codec[Short] = new NumericCodec[Short] {
    override def from(number: Number): Short = number.shortValue()
  }

  val byte: Codec[Byte] = new NumericCodec[Byte] {
    override def from(number: Number): Byte = number.byteValue()
  }

  val double: Codec[Double] = new NumericCodec[Double] {
    override def from(number: Number): Double = number.doubleValue()
  }

  val float: Codec[Float] = new NumericCodec[Float] {
    override def from(number: Number): Float = number.floatValue()
  }

  val bool: Codec[Boolean] = new Codec[Boolean] {
    override def decode[T](dataOps: DataOps[T], source: T): Try[Boolean] =
      Try[Boolean] {
        dataOps.getBoolean(source)
      }

    override def encode[T](dataOps: DataOps[T], value: Boolean): T = dataOps.createBoolean(value)
  }

  val string: Codec[String] = new Codec[String] {
    override def decode[T](dataOps: DataOps[T], source: T): Try[String] =
      Try[String] {
        dataOps.getString(source)
      }

    override def encode[T](dataOps: DataOps[T], value: String): T = dataOps.createString(value)
  }

  implicit class FieldCodecCast[OV](val codec: Codec[OV]) {
    def fieldOf[R](name: String, getter: R => OV): FieldCodec[OV, R] = FieldCodec(name, codec, getter)
  }

  implicit class OptionCodecCast[OV](val codec: Codec[OV]) {
    def optional: Codec[Option[OV]] = new Codec[Option[OV]] {
      override def decode[T](dataOps: DataOps[T], source: T): Try[Option[OV]] =
        Try[Option[OV]] {
          Option[OV](codec.decode(dataOps, source).get)
        }

      override def encode[T](dataOps: DataOps[T], value: Option[OV]): T =
        value match
          case Some(succ) => codec.encode(dataOps, succ)
          case None => dataOps.createNull()
    }
  }
}