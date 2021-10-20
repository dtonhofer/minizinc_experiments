# Stats Collection

A program to collect statistics about running MiniZinc on a single model
with several datafiles and with varying annotations to try different
"variable selection" and "domain splitting" strategies.

This problem helps in solving workshop 9 from week 1 of the course 
[Solving Algorithms for Discrete Optimization](https://www.coursera.org/learn/solving-algorithms-discrete-optimization)
by Prof. Jimmy Ho Man Lee and Prof. Peter James Stuckey.

In that workship one wants to find a good variable selection strategy
and domain splitting strategy. To do so, one has to manually edit the
"annotation" of the `solve` instruction which tells MiniZinc to select 
particular search strategies.

Instead of doing this manually, 
we write a Perl program that runs MiniZinc (on the command line) with a slightly
modified version of the original assignment model. The modified model uses a parameter
for the search strategy annotation instead of having a hardcoded one that needs
to be changed by the user.

A CVS file `result.txt` with results is created. That file can then be further processed, for example in RStudio
or simply in Excel.

The file tree is as follows:

```
.
├── collect.perl       ... Perl program to run MiniZinc, writes result.txt.
├── collect.cfg        ... YAML file which tells collect.perl what to do.
├── run_collect.sh     ... Onleiner script to start collect.perl
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
│   │                             captured in logfiles that appear in this directory.
│   │                             The files are removed after the MiniZinc
│   │                             process terminates successfully. You can just
│   │                             remove this directory to clean up, it is
│   │                             recreated on need.
│   │
│   └── prepare_modded_prepare7_1_BlBl.out
│
├── models
│   └── prepare_modded.mzn    ... Modified model file from Workshop 9.
│
├── modules                   ... Perl modules that are loaded by collect.perl
│   └── Mzn
│       ├── Forking.pm
│       ├── ReadYaml.pm
│       ├── ResultFiling.pm
│       └── TaskQueue.pm
|
├── result.txt                ... Resulting collected information (CSV file).
└── result.ods                ... As above, but manually loaded into LibreOffice Calc
```

MiniZinc subprocesses write their stdout and stderr to files in directory `logs`
and remove them again if all went well and a valid line could be added to `result.txt`.
Otherwise the files remain in the `logs` directory for manual inspection.

If the file `result.txt` already exists if `collect_result.perl` is started,
it is renamed by adding an integer index. This avoids accidental destruction
of data that might have taken a few hours to collect.

The `result.txt` file in this repo contains the the result of running `collect_result.perl`
on the given model and with the given parameter files on this machine:
_4-core Intel(R) Xeon(R) CPU W3520 @ 2.67GHz, Linux Fedora 33, 24 GiB RAM_.

## Configuration: collect.cfg

`collect.cfg` is a [YAML](https://en.wikipedia.org/wiki/YAML) file that specifies what
`collect.perl` should do. It is rather self-explanatory.

## Running it

A shown in `run_collect.sh`, the program takes a number of options that may help
in understanding what it does or in debugging:

By default one might call it like this

```
perl collect.perl --cfg=collect.cfg --parallel=4 --keeplogs 
```

To run 4 MiniZinc processes in parallel and not destroy the logs in the `logs` directory even if successful.

One can switch on debugging by adding these flags:

- `--debugcfg`: Print information about the configuration read and interpreted.
- `--debugtasks`: Print information about the MiniZinc instantiations ("tasks") that are about to be run.
- `--debugresults`:  For each task, print information about collected results. 

## Fields in the output `result.txt`

The file [`result.txt`](/stats_collection/result.txt) lists the info collected, one MiniZinc run per line. Missing values
are denoted by `NA` as is the custom in R (the statistical package).

- `model`: A string derived from the name of the model file used for this run, to be used for sorting and searching.
- `data`: A string derived from the name of the data file used for this run, to be used for sorting and searching.
- `annotation`: The actual annotation text used when running the model. The actual values of the strategies employed are found in this string.
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

## Info on search strategy annotations 

See

- [MiniZinc manual: Search](https://www.minizinc.org/doc-2.5.5/en/mzn_search.html)
- [MiniZinc manual: Search annotations](https://www.minizinc.org/doc-2.5.5/en/fzn-spec.html?highlight=dom_w_deg#search-annotations) (complete list of currently available search annotations)
- [CP Profiler](https://www.minizinc.org/doc-2.5.5/en/cpprofiler.html)

### Variable selection strategy

- `input_order`: choose in order from the array
- `first_fail`: choose the variable with the smallest domain size
- `anti_first_fail`: choose the variable with the largest domain
- `smallest`: choose the variable with the smallest value in its domain
- `largest`: choose the variable with the largest value in its domain
- `occurrence`: choose the variable with the largest number of attached constraints
- `most_constrained`: choose the variable with the smallest domain, breaking ties using the number of constraints
- `max_regret`: choose the variable with the largest difference between the two smallest values in its domain.
- `dom_w_deg`: choose the variable with the smallest value of domain size divided by weighted degree, which is the number of times it has been in a constraint that caused failure earlier in the search

### Domain splitting strategy

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

