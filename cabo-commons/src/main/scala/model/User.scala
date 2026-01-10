package model

import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(value = classOf[UserBase], name = "userBase")
  ))
trait User:
  def userID: String
  def name: String

case class UserBase(userID: String, name: String) extends User


