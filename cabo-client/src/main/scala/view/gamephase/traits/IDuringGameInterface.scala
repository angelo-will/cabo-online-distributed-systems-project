package view.gamephase.traits

import model.Game.GameInProgress
import model.{Card, PlayerPlaying, TurnLog}
import view.gamephase.*

trait IDuringGameInterface extends IGameInfoView
  with IGamePhaseStatesView
  with ICardActionView
  with IPowerInteractionView
  with IConnectionsInfo