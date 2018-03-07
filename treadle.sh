#!/usr/bin/env bash
sbt -mem 4000 "runMain treadle.TreadleRepl $*"
