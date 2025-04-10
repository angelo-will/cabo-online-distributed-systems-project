package model

trait AddressableInLobby:
  def address: String

trait WithHand extends AddressableInLobby:
  def hand: Hand

case class PlayerInLobby(userID: String, name: String, address: String) extends AddressableInLobby with User:
  override def userId: String = userID
  override def nome: String = name
  
case class PlayerPlaying(userID: String, name: String, address: String, hand: Hand) extends WithHand with User:
  override def userId: String = userID
  override def nome: String = name