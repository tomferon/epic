Epic is a simple embedded purely functional programming language. It is meant to
be used in the same way Lua or Javascript are sometimes used. A Haskell program
can accept code written in Epic from users and run it with two guarantees:

* It is typechecked and runtime errors should not occur.
* It is pure so there are no side-effects. It is impossible for the user to
  start reading from disk or sending packets for example.

However, it does NOT guarantee that the program terminates in a reasonable
amount of time (or at all because of `fix`) and that the program doesn't use too
much memory. These two issues need to be taken care of by the host program.

# Epic language

Epic is an implementation of System F.
