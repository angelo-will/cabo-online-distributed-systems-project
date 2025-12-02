package messages

object ViewUserCommandMessages {
  case class OwnCardSelected(index: Int) extends IViewUserCommand

  case class AdversaryCardSelected(adversaryID: String, index: Int) extends IViewUserCommand

  case class DeckSelected() extends IViewUserCommand

  case class DiscardStackSelected() extends IViewUserCommand

  case class DiscardCardDrawnSelected() extends IViewUserCommand

  case class CallCaboSelected() extends IViewUserCommand

  case class EndTurnSelected() extends IViewUserCommand

  case class ExitSelected() extends IViewUserCommand

  case class ConsultingResultsEnded() extends IViewUserCommand
}
