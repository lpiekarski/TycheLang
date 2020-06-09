#!/bin/bash
export STACK="/home/students/inf/PUBLIC/MRJP/Stack/stack --system-ghc --resolver lts-13.19"
$STACK setup
$STACK config set system-ghc --global true
$STACK config set resolver lts-13.19
$STACK upgrade --force-download
export PATH=$($STACK path --local-bin):$PATH
$STACK install random
$STACK ghc -- --make Tyche/Main.hs -o interpreter
