package io.github.tt432

import com.google.gson.{Gson, JsonObject}

import scala.util.Success

/**
 * @author TT432
 */

class TestX(val s1: String, val i1: Int, val i2: Int, val s2: String) {
  override def toString = s"TestX(s1=$s1, i1=$i1, i2=$i2, s2=$s2)"
}

@main
def main(): Unit = {
  val codec =
    (Codec.string.fieldOf("a", r => r.s1)
      &: Codec.int.fieldOf("b", r => r.i1)
      &: Codec.int.fieldOf("c", r => r.i2)
      &: Codec.string.fieldOf("d", r => r.s2)
      &: CodecBuilder.result[TestX]
      )(
      s1 => i1 => i2 => s2 => TestX(s1, i1, i2, s2)
    )

  val testx1 = codec.decode(GsonOps, Gson().fromJson(
    """
      |{
      |  "a" : "string1",
      |  "b" : 123,
      |  "c" : 18263,
      |  "d" : "string2"
      |}
      |""".stripMargin, classOf[JsonObject]))

  testx1 match
    case Success(v) => println(v)
}