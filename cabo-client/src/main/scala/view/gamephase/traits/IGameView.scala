package view.gamephase.traits

import view.gamephase.*

trait IGameView extends IGameInfoView
  with IGamePhaseStatesView
  with ICardActionView
  with IPowerInteractionView
  with IConnectionsInfo