package io.github.tt432

import com.google.gson.*

import java.lang.Number
import scala.jdk.CollectionConverters.*

/**
 * @author TT432
 */

object GsonOps extends DataOps[JsonElement] {
  override def createNull(): JsonElement = JsonNull.INSTANCE

  override def getNumber[R](value: JsonElement): Number =
    value match
      case jp: JsonPrimitive => jp.getAsNumber
      case _ => throw ParseException(s"$value is not a number")

  override def createNumber[N <: Number](value: N): JsonElement = JsonPrimitive(value)

  override def getString(value: JsonElement): String =
    value match
      case jp: JsonPrimitive => jp.getAsString
      case _ => throw ParseException(s"$value is not a string")

  override def createString(value: String): JsonElement = JsonPrimitive(value)

  override def getBoolean(value: JsonElement): Boolean =
    value match
      case jp: JsonPrimitive => jp.getAsBoolean
      case _ => throw ParseException(s"$value is not a boolean")

  override def createBoolean(value: Boolean): JsonElement = JsonPrimitive(value)

  override def getMap(value: JsonElement): Map[String, JsonElement] =
    value match
      case jo: JsonObject => jo.asMap().asScala.toMap
      case _ => throw ParseException(s"$value is not a object")

  override def createMap[V <: Map[String, JsonElement]](value: V): JsonElement = {
    val jo = JsonObject()

    value.foreach((k, v) => jo.add(k, v))

    jo
  }

  override def getList(value: JsonElement): List[JsonElement] =
    value match
      case ja: JsonArray => ja.asList().asScala.toList
      case _ => throw ParseException(s"$value is not a array")

  override def createList[V <: List[JsonElement]](value: V): JsonElement = {
    val ja = JsonArray()

    value.foreach(v => ja.add(v))

    ja
  }
}