#!/bin/bash

# The final "$@" passes any additional command line args given to this
# script on to "collect.perl"

cfgfile=cfg/explained.cfg
mydir=$(dirname $0)

perl $mydir/collect/collect.perl "--cfg=$cfgfile" "--workdir=$mydir" --parallel=4 "$@"

# Possible debugging flags that can be additionally passed:
#
# --debugcfg
# --debugtasks
# --debugresults
# --keeplogs
#
# and to stop before running MiniZinc:
#
# --dryrun

