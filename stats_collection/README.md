# Stats Collection

A Perl program to collect statistics about running MiniZinc on one or several models
with several datafiles and with varying annotations to try different
"variable selection" and "domain splitting" strategies. Additionally, one can
specify different "restart strategies"

This problem helps in solving _workshop 9_ from _week 1_ of the course 
[Solving Algorithms for Discrete Optimization](https://www.coursera.org/learn/solving-algorithms-discrete-optimization)
by Prof. Jimmy Ho Man Lee and Prof. Peter James Stuckey.

In that workship one wants to find a good variable selection strategy
and domain splitting strategy. To do so, one has to manually edit the
"annotation" of the `solve` instruction which tells MiniZinc (or more
accurately, Gecode) to select particular search strategies.

Instead of doing this manually, we write a Perl program that runs MiniZinc
(on the command line) with a slightly modified version of the original 
assignment model. The modified model has a parameter for the "search strategy
annotation instead" of a hardcoded annotation. Additionally, it features a
parameter for a "restart strategy annotation".

A CVS file `result.txt` with results is created. That file can then be further
processed, for example in RStudio or simply in Excel.

The file tree is as follows:

```
├── cfg
│   └── explained.cfg  ... YAML file which tells collect.perl what to do.
│                          Contains explanations. This is what the user
│                          should edit.
│
├── collect
│   ├── collect.perl   ... Perl program to run MiniZinc, writes result.txt.
│   └── modules        ... Perl modules used by "collect.perl"
│       └── Mzn
│           ├── Forking.pm
│           ├── Helpers.pm
│           ├── ReadYamlAliases.pm
│           ├── ReadYamlConfigCollection.pm
│           ├── ReadYamlDatafiles.pm
│           ├── ReadYamlDefaults.pm
│           ├── ReadYamlModelfiles.pm
│           ├── ReadYaml.pm
│           ├── ResultFiling.pm
│           └── TaskQueue.pm
│
├── data               ... Directory with the datafiles from Workshop 9. 
│   ├── prepare10.dzn
│   ├── prepare11.dzn
│   ├── prepare12.dzn
│   ├── prepare13.dzn
│   ├── prepare14.dzn
│   ├── prepare1.dzn
│   ├── prepare2.dzn
│   ├── prepare3.dzn
│   ├── prepare4.dzn
│   ├── prepare5.dzn
│   ├── prepare6.dzn
│   ├── prepare7.dzn
│   ├── prepare8.dzn
│   └── prepare9.dzn
│
├── logs                      ... The stdout/sdterr of MiniZinc processes is
│                                 captured in logfiles that appear in this directory.
│                                 The files are removed after the MiniZinc
│                                 process terminates successfully. You can just
│                                 remove this directory to clean up, it is
│                                 recreated on need.
│
├── models
│   └── prepare_modded.mzn    ... Modified model file from Workshop 9.
├── README.md
├── result.ods
├── result.txt
└── run_collect.sh            ... Starter script.
```

MiniZinc subprocesses started by `collect.perl`write their stdout and stderr to files in directory `logs`
and remove them again if all went well and a valid line could be added to `result.txt`.
Otherwise the files remain in the `logs` directory for manual inspection.

If the file `result.txt` already exists when `collect_result.perl` is started,
it is renamed by adding an integer index. This avoids accidental destruction
of data that might have taken a few hours to collect.

## Configuration file

The file `explained.cfg` is a [YAML](https://en.wikipedia.org/wiki/YAML) file that specifies what
`collect.perl` should do: which model files and data files it should process how and
what the parameters should be set to. It is rather self-explanatory.

## Running it

A shown in `run_collect.sh`, the program takes a number of options that may help
in understanding what it does or in debugging:

By default one might call it like this

```
perl collect.perl --cfg=cfg/explained.cfg --parallel=4 --keeplogs 
```

To run 4 MiniZinc processes in parallel and not destroy the logs in the `logs` directory even if successful.

The `--help` option brings more:

```
./run_collect.sh --help
--cfg=<YAML config file>      : Set configuration file.
--workdir=<toplevel work dir> : Set the directory under which "model file"
                                and "data files" reside. If missing, the
                                current directory is used.
--parallel=<number>           : Start 'number' parallel MiniZinc processes
                                to work off all the tasks. Default is 1.
                                (this is not the "number of threads" arg
                                that gecode understands)
--debugcfg                    : Print info about config read & processed.
--debugtasks                  : Print tasks (arg sets for MiniZinc executions).
--debugresults                : Print the results obtained from MiniZinc.
--scramble                    : Scramble the MiniZinc task queue prior to
                                running MiniZinc tasks for more randomness.
--keeplogs                    : Do not delete MiniZinc logs after completion.
--dryrun                      : Just print the tasks that would be run, then
                                exit.
```

## Fields in the output `result.txt`

The file [`result.txt`](/stats_collection/result.txt) lists the info collected, one MiniZinc run per line. Missing values
are denoted by `NA` as is the custom in R (the statistical package).

- `model`: A string derived from the name of the model file used for this run, to be used for sorting and searching.
- `data`: A string derived from the name of the data file used for this run, to be used for sorting and searching.
- `config_name`: The name of the _configuration_ from the config file to which this line corresponds.
- `application_name`: The name of the _application_ (a sub-element of a configuration) from the config file to which this line corresponds.
- `search`: Full search annotation text used, or `NA`.
- `restart`: Full restart annotation text used, or `NA`. 
- `round`: The index of the "round" (starting from 1) if the exact same problem is run several times (useful in case of random selection).
- `rounds`: Number of rounds that will be run. Generally 1. 
- `limit_s`: Time limit given to MiniZinc in seconds; the best solution (in this case, "shortest schedule solution") found within that limit counts.
- `duration_s`: Time spent processing in seconds, as determined by the collection script.
- `obj`: **objective value**, in this case best (shortest) overall duration of the schedule found. If nothing was found, we write `NA`, as is the custom in `R`.

Next come performance values output by MiniZinc due to the `--statistics` option.
Refer to [Statistics Output](https://www.minizinc.org/doc-2.5.5/en/fzn-spec.html#statistics-output) in the manual

- `init_time_s`: Time spent initializing in seconds. Can be disregarded, as it's always below 0.1s.
- `solve_time_s`: Time spent solving in seconds. Cannot be larger than `limit_s`, but can be smaller.
- `solutions`: Solutions found during optimization. If this is 0, it's a bust and `obj` should contain `NA`.
- `variables`: Number of variables created from the problem statement. (In this case, varies from 71 to 2451 depending on problem.)
- `propagators`: Number of variables created from the problem statement. (In this case, varies from 78 to 2891 depending on problem.)
- `propagations`: Number of propagator invocations. (In this case, maximum observed value overall is 86 million.)
- `nodes`: Number of search nodes. (In this case, maximum observed value overall is 2 million.) 
- `failures`: Number of leaf nodes that were failed. (In this case, maximum observed value overall is above 1 million.)
- `restarts`: Number of times the solver restarted the search (jumped back to the root search node). (Here, always 0.)
- `peak_depth`: Peak depth of search tree reached. (In this case, `prepare14` reaches 1300.)
- `num_solutions`: The `nSolutions` value, this should be the "number of solutions output". It think.

## Search Strategies

There is a set of keywords for "variable selection strategy" (VSS, what variable to select next during search), called "varchoiceannotation" or
"value choice strategy" in the MiniZinc manual, as well as a set for "domain # splitting strategy" (DSS, how to constrain the domain on the left
branch of the binary decision tree), called "assignmentannotation" in the MiniZinc manual. These are combined inside an `int_search($VSS,$DSS)`
annotation.

- [MiniZinc manual: Search](https://www.minizinc.org/doc-2.5.5/en/mzn_search.html)
- [MiniZinc manual: Search annotations](https://www.minizinc.org/doc-2.5.5/en/fzn-spec.html#search-annotations) (complete list of currently available search annotations)
- [CP Profiler](https://www.minizinc.org/doc-2.5.5/en/cpprofiler.html)
- [Additional Gecode search annotations](https://www.minizinc.org/doc-2.5.5/en/lib-gecode.html#additional-gecode-search-annotations)

### Known Variable Selection Strategies (VSS)

- `input_order`: choose in order from the array
- `first_fail`: choose the variable with the smallest domain size
- `anti_first_fail`: choose the variable with the largest domain
- `smallest`: choose the variable with the smallest value in its domain
- `largest`: choose the variable with the largest value in its domain
- `occurrence`: choose the variable with the largest number of attached constraints
- `most_constrained`: choose the variable with the smallest domain, breaking ties using the number of constraints
- `max_regret`: choose the variable with the largest difference between the two smallest values in its domain.
- `dom_w_deg`: choose the variable with the smallest value of domain size divided by weighted degree, which is the number of times it has been in a constraint that caused failure earlier in the search

Gecode also allows these:

- `activity_max`: Select variable with largest activity count.
- `activity_min`: Select variable with smallest activity count.
- `activity_size_max`: Select variable with largest activity count divided by domain size.
- `activity_size_min`: Select variable with smallest activity count divided by domain size
- `afc_max`: Select variable with largest accumulated failure count.
- `afc_min`: Select variable with smallest accumulated failure count.
- `afc_size_max`: Select variable with largest accumulated failure count divided by domain size.
- `afc_size_min`: Select variable with smallest accumulated failure count divided by domain size.
- `random`: Select random variable.

### Known Domain Splitting Strategies (DSS)

A variable's domain is always split in two. Thus the search tree is a binary tree.

- `indomain_min`: assign the smallest value in the variable's domain
- `indomain_max`: assign the largest value in the variable's domain
- `indomain_middle`: assign the value in the variable’s domain closest to the mean of its current bounds
- `indomain_median`: assign the middle value in the variable’s domain
- `indomain`: nondeterministically assign values to the variable in ascending order
- `indomain_random`: assign a random value from the variable’s domain
- `indomain_split`: bisect the variable’s domain, excluding the upper half first.
- `indomain_reverse_split`: bisect the variable’s domain, excluding the lower half first.
- `indomain_interval`: if the variable’s domain consists of several contiguous intervals, reduce the domain to the first interval. Otherwise just split the variable’s domain.

## Restart Strategies

The "restart" annotation is simpler than the "search" annotation and does not use keywords for configuration, just integers and floats. 
The following are known:

- `restart_none`: Do not restart (the default).
- `restart_constant(int: scale)`: Restart after constant number of nodes "scale".
- `restart_geometric(float: base,int: scale)` : Restart with geometric sequence with parameters base and scale.
- `restart_linear(int: scale)` : Restart with linear sequence scaled by scale.
- `restart_luby(int: scale)` : Restart with Luby sequence scaled by scale

 A restart is triggered when (presumably) an limiting number of
additional search tree nodes have been visited, with the limiting sequence
either a constant, or an appropriately scaled exponential (geometric), linear
"Luby" sequence.

The Luby sequence (named after A. Luby and described in "Optimal Speedup of Las Vegas Algorithms", 1993-02) is as follows:

```
1 1 2 1 1 2 4 1 1 2 1 1 2 4 8 1 ..
```

[Information about restart annotations from the manual](https://www.minizinc.org/doc-2.5.5/en/lib-stdlib.html#restart-annotations)

## See also

- [MiniZinc command line tool](https://www.minizinc.org/doc-2.5.5/en/command_line.html)

## Python interface

Jip J. Dekker says: 

> We have a [MiniZinc Python interface](https://minizinc-python.readthedocs.io/en/latest/),
> that should make interacting with MiniZinc from script a lot easier 
> (including the collection of statistics). We even have a MiniZinc benchmarking tool, but this 
> is mainly aimed at clusters running [Slurm](https://slurm.schedmd.com/overview.html) at the moment.

## Bugs

They probably exist. 

