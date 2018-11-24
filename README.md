# The Hask Runner

## Overview

This is a simple platformer game where you have to navigate you cube through various obstacles.
The only thing you control in this game is gravity direction. Try to survive for as long as possible.

## Controls

Press Space key to reverse gravity

## OSX build

```
stack build
```

## Run

```
stack exec the-hask-runner
```

OR

```
stack repl
main
```

## Running Scoreboard Server

For the server you need redis listening on port 6379. To build and start a server run:
```
stack build && stack exec hask-server
```

The server listens on port 3000
