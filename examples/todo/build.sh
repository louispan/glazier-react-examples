#!/bin/sh
cabal --ghc v2-build build && `cabal v2-exec which build` "$@"
