package com.topaz.event

/**
  * Some ids will be nullable so that new entities can be created without knowing the id.
  */
trait NullableId {
  def isNull: Boolean
  def id: Int
  def withNewId(id: Int): NullableId
  // The name is used to as the key space for the generator sequence.
  // So, for example, the TransactionId name is the same for both the system and blotter
  // transaction stores so that we create ids that are unique in both.
  def name: String
}

trait ValueWithNullableId {
  def identifier: NullableId
  def withNewId(id: NullableId): ValueWithNullableId
}
