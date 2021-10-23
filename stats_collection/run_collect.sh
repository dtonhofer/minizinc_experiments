#!/bin/bash

# The final "$@" passes any additional command line args given to this
# script on to "collect.perl"

perl collect/collect.perl --cfg=cfg/simple.cfg --parallel=4 "$@"

# Debugging flags:
# --debugcfg
# --debugtasks
# --debugresults
# --keeplogs

