#!/usr/bin/env bash
sbt -no-colors -mem 4000 "runMain treadle.TreadleRepl $*"
