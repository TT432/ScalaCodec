package io.github.tt432

/**
 * @author TT432
 */

trait DataOps[T] {
  def createNull(): T

  def getNumber[R](value: T): Number

  def createNumber[N <: Number](value: N): T

  def getString(value: T): String

  def createString(value: String): T

  def getBoolean(value: T): Boolean

  def createBoolean(value: Boolean): T

  def getMap(value: T): Map[String, T]

  def createMap[V <: Map[String, T]](value: V): T

  def getList(value: T): List[T]

  def createList[V <: List[T]](value: V): T
}