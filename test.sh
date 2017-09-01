#!/usr/bin/env bash

#cabal test --show-details=streaming

# from https://hspec.github.io/options.html
cabal test --show-details=direct --test-option=--maximum-generated-tests=10000
#cabal test --show-details=direct --test-option=--format=progress --test-option=--maximum-generated-tests=10000
