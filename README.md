# Cabo Online - Distributed Systems Project

Project created for Distributed Systems course.

## Overview

The goal of this project is to develop a desktop application that allows users to play the online version of *"Cabo"*, a turn-based card game. Details about the game itself and how it works can be found in the [Rules of the game](https://www.google.com/search?q=%23rules-of-the-game) section.

The application runs on PC and provides a GUI to allow user interaction in every step of the system, from creating or joining a game, to the single actions performed during a turn.

Games can be private or public: in the first case, other players can join only by using a unique code; in the second case, the game is shared through a lobby server and becomes visible to all players in the system.

The server does not work as a dispatcher of messages between players; it is only used to obtain a reference to the host to start the communication.

Therefore, the application is structured as a peer-to-peer (P2P) system, meaning that the players participating in a game are connected directly to each other.

Without a centralized server, it is necessary that the information and data shared between players are always consistent and up to date. In particular, during a match, the game state (which includes the players' hands and the table situation) must always be the same for every player at every turn, to ensure that the actions chosen by a player are coherent and correct with respect to the current situation.

### Roles

#### Player

The player will handle game creation and joining, and will send commands to play the game.

#### Lobby server

The lobby server is used to share games that are not set as private, so that other players can find them. It handles requests such as publishing a game, getting information about shared games, and removing a previously shared game.

Since this is a P2P system, players do not communicate with each other through the server; instead, it only serves as a means to retrieve their contact information.

### Rules of the game

#### Cards

Cabo can be played with specific or generic cards. In this project, French cards will be used.

| Card Rank | Score | Special Power |
| --- | --- | --- |
| Ace | 1 | No Power |
| Two | 2 | No Power |
| Three | 3 | No Power |
| Four | 4 | No Power |
| Five | 5 | No Power |
| Six | 6 | No Power |
| Seven | 7 | No Power |
| Eight | 8 | No Power |
| Nine | 9 | No Power |
| Ten | 0 | No Power |
| Jack | 10 | See one of your cards |
| Queen | 10 | See one card of one opponent's cards |
| King | 10 | Exchange one of your cards with one card from an opponent |

*Cards scores and special powers. The power to exchange cards does not allow to see them.*

#### How it works

Cabo is a turn-based card game for two to five players.

The game begins by dealing four face-down cards to each player. A single card is placed face-up to start the **discard pile**, while the remaining cards form the **deck** (draw pile), placed face-down in the center. Before the first turn begins, each player is allowed to peek at two of their own cards.

**The goal of the game is to achieve the lowest total score among the four cards in hand.**

Once all players have viewed their two cards, the first player begins their turn. Each turn consists of the following steps:

1. **Draw a card:** Draw the top card from either the deck or the discard pile. If drawn from the deck, the card is revealed to all players.
2. **Use card powers:** If a card with a special power is drawn from the deck, the player may use its ability:
* **See one of your cards:** reveal one of your cards only to you.
* **See one card of an opponent:** choose an opponent and one of their cards to reveal only to you.
* **Exchange one of your cards with one of an opponent:** choose one of your cards and one card of an opponent; the selected cards are exchanged, but nobody will know their value.


3. **Discard or Swap:** If the card was drawn from the deck, the player decides whether to keep it (replacing one of their current cards) or discard it. If the card was drawn from the discard pile, it must be kept and cannot be discarded immediately. The discarded card is placed face-up on the discard pile.
4. **End turn or Call Cabo:** The player decides to end their turn or "Call Cabo."

Once *"Cabo"* is called, a final round begins. The game ends when the turn reaches the player who called Cabo again. Alternatively, the game ends immediately if the deck is exhausted.

## How to run

To install the software, Java 17+ and the Simple Build Tool (sbt) ([https://www.scala-sbt.org/download](https://www.scala-sbt.org/download)) are needed.

Download the code from the GitHub repository:

```bash
git clone [https://github.com/angelo-will/cabo-online-distributed-systems-project.git](https://github.com/angelo-will/cabo-online-distributed-systems-project.git)

```

Run:

```bash
sbt install

```

Three `.jar` files will be generated: `seed.jar`, `server.jar`, and `client.jar`.

The seed module must be started first so that Akka Cluster can work:

```bash
java -jar seed.jar

```

Information about cluster creation will be printed (e.g., nodes joining the cluster and being marked as `UP`).

The server is not required for private games, as the game code can be used to find a match created by a host. However, it can be started with:

```bash
java -jar server.jar

```

It prints cluster and server information whenever a game is published, updated, or deleted.

The client is the player application and can be started with:

```bash
java -jar client.jar

```

For debugging purposes, game-related logs are printed (e.g., opponents' cards are visible, and the deck structure is shown).
