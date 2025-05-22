package view.ui

enum InitialPhaseNamesEnum(val name: String):
  case WelcomePanel extends InitialPhaseNamesEnum("welcomePanel")
  case CreateGamePanel extends InitialPhaseNamesEnum("createGamePanel")
  case JoinGamePanel extends InitialPhaseNamesEnum("joinGameScreen")
  case JoinGameWithLinkPanel extends InitialPhaseNamesEnum("joinGameByLinkScreen")

