package model

trait User:
  def userId: String
  def nome: String

case class UserBase(userId: String, nome: String) extends User


