# tic-taskell

That's tic-tac-toe and Haskell. This is a server to run a game of tic-tac-toe
based on POST requests from its users. The state of the game is retrieved
through clients sending a GET request to the server.

### Installation

To download and run the project:

```bash
>$ git clone http://github.com/crockeo/tic-taskell.
>$ cd tic-taskell
>$ cabal sandbox init
>$ cabal install --only-dependencies
>$ cabal run
```

To build / open a REPL just run the matching cabal commands.
