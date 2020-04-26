package com.topaz.utils

import enumeratum.values.{IntEnum, IntEnumEntry}

import scala.collection.breakOut

trait NamedIntEnumEntry extends IntEnumEntry {
  def value: Int
  def name: String
}


/**
  * Allows resolution of enums via int value _and_ string name.
  */
trait NamedIntEnum[T <: NamedIntEnumEntry] extends IntEnum[T] {
  protected def caseSensitive = true
  private lazy val valuesByName: Map[String, T] = values.map{ 
    v => (if(caseSensitive) v.name else v.name.toLowerCase) -> v
  }(breakOut)
  def withName(name: String): T = valuesByName(if(caseSensitive) name else name.toLowerCase)
  def maybeWithName(name: String): Option[T] = valuesByName.get(if(caseSensitive) name else name.toLowerCase)
}

