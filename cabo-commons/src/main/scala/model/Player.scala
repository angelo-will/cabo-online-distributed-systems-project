package model

import akka.actor.typed.ActorRef
import utils.Message

trait WithHand:
  def hand: Hand

case class PlayerInLobby(userID: String, name: String, address: ActorRef[Message]) extends User:
  override def userId: String = userID

  override def nome: String = name

object PlayerPlaying:

  /**
   * Retrieves the PlayerPlaying object from a list of players based on their unique userID.
   *
   * @param userID  The unique identifier of the player to retrieve.
   * @param players The list of PlayerPlaying objects to search within.
   * @return The PlayerPlaying object with the matching userID.
   * @throws NoSuchElementException if no player with the given userID is found in the list.
   */
  def getPlayerWithID(userID: String, players: List[PlayerPlaying]): PlayerPlaying =
    players.find(_.userID == userID).get

  /**
   * Retrieves the Hand object of a player from a list of players based on their unique userID.
   *
   * @param userID  The unique identifier of the player whose hand to retrieve.
   * @param players The list of PlayerPlaying objects to search within.
   * @return The Hand object of the player with the matching userID.
   * @throws NoSuchElementException if no player with the given userID is found in the list.
   */
  def getHandOfPlayerWithID(userID: String, players: List[PlayerPlaying]): Hand =
    getPlayerWithID(userID, players).hand

  /**
   * Creates a new list of PlayerPlaying objects where the hand of the player with the specified userID
   * has been replaced with the provided hand. All other players in the list remain unchanged.
   *
   * @param userID  The unique identifier of the player whose hand to replace.
   * @param hand    The new Hand object to assign to the player.
   * @param players The original list of PlayerPlaying objects.
   * @return A new list of PlayerPlaying objects with the updated hand for the specified player.
   * @throws NoSuchElementException if no player with the given userID is found in the list.
   */
  def replaceHandOfPlayerWithID(userID: String, hand: Hand, players: List[PlayerPlaying]): List[PlayerPlaying] =
    if !players.exists(_.userID == userID) then
      throw new NoSuchElementException(s"There isn't a player with this id $userID")
    players.map(p => if p.userID == userID then p.copy(hand = hand) else p)

  /**
   * Creates a new list of PlayerPlaying objects where the nth card in the hand of the player
   * with the specified userID has been replaced with the provided card. All other players
   * in the list remain unchanged. This method assumes that the `PlayerPlaying` class has
   * a method `replaceNthCard(index: Int, card: Card)` that performs the actual replacement
   * on the player's hand.
   *
   * @param userID  The unique identifier of the player whose card to replace.
   * @param card    The new Card object to replace the existing card.
   * @param index   The zero-based index of the card to replace in the player's hand.
   * @param players The original list of PlayerPlaying objects.
   * @return A new list of PlayerPlaying objects with the updated card in the specified player's hand.
   * @throws NoSuchElementException if no player with the given userID is found in the list.
   */
  def replaceNthCardOfPlayerWithID(userID: String, card: Card, index: Int, players: List[PlayerPlaying]): List[PlayerPlaying] =
    if !players.exists(_.userID == userID) then
      throw new NoSuchElementException(s"There isn't a player with this id $userID")
    players.map(p => if p.userID == userID then p.copy(hand = Hand(p.hand.cards.updated(index, card))) else p)


case class PlayerPlaying(userID: String, name: String, hand: Hand) extends WithHand with User:
  override def userId: String = userID

  override def nome: String = name

  def replaceNthCard(index: Int, card: Card): PlayerPlaying =
    val newHand = Hand(hand.cards.updated(index, card))
    this.copy(hand = newHand)