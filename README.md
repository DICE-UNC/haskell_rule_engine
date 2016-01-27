# haskell_rule_engine

How to build:

gcc -std=c++11 -I. -I/usr/include -I/usr/lib/ghc/include libre-haskell.cpp
ghc -dynamic -shared -fPIC RE.hs
