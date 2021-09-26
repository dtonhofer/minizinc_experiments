# Stats Collection

Collecting statistics over several runs of MiniZinc solving a scheduling problem.

This problem is from week 1 of the course [Solving Algorithms for Discrete Optimization](https://www.coursera.org/learn/solving-algorithms-discrete-optimization)
by Prof. Jimmy Ho Man Lee and Prof. Peter James Stuckey.

The idea is to tune the variable selection and value selection hints and take some
conclusion as to what's best.

But why do it manually?

We write a Perl program that runs MiniZinc (on the command line) with a slightly modified version of the model.
The modified model uses a parameter for the annotation hinting at how to 
select variables and values.

A file with results is created. That file can then be further processed, for example in RStudio.

The file structure is as follows:

```
.
├── collect_result.perl   - Perl program to run MiniZinc, writes result.txt
├── data                  - Problem parameter files from Workshop 9
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
├── log                   - MiniZinc subprocesses dump their stdout/stderr into this directory
├── prepare_modded.mzn    - Modified model file from Workshop 9
├── result.txt            - Collected information as CSV file
└── README.md
```

MiniZinc subprocesses write their stdout and stderr to files in directory `log`
and remove them again if all went well and a valid line could be added to `result.txt`.
Otherwise the files remain in the `log` directory for manual inspection.

If the file `result.txt` already exists if `collect_result.perl` is started,
it is renamed by adding an integer index, thus avoiding accidental destruction
of data that might have taken a few hours to collect.

The `result.txt` file found here is the result of running `collect_result.perl`
on a 4-core Intel(R) Xeon(R) CPU W3520 @ 2.67GHz, Linux Fedora 33, 24 GiB RAM.

## Fields in the output `result.txt`

The file `result.txt` lists the info collected, one MiniZinc run per line.

- `base`: The basename of parameter/config file.
- `round`: The index of the "round" (starting from 0) if exactly the same problem is run several times.
- `var_sel`: The setting for the "variable selection" strategy (in this case, applied to the sought schedule "start times", variables `s`).
- `val_sel`: The setting for the "value selection" strategy (in this case, applied to the sought schedule "start times", variables `s`).
- `limit_s`: Time limit given to MiniZinc in seconds; the best solution (in this case, shortest schedule solution) found within that limit counts.
- `duration_s`: Time spent processing in seconds, as determined by the collection script.
- **`makespan`**: Best overall duration of the schedule found. The smaller the better. If nothing was found, we write `NA`, as is the custom in `R`.

Next come performance values output by MiniZinc due to the `--statistics` option.
Refer to [Statistics Output](https://www.minizinc.org/doc-2.5.5/en/fzn-spec.html#statistics-output) in the manual

- `init_time_s`: Time spent initializing in seconds. Can be disregarded, as it's always below 0.1s.
- `solve_time_s`: Time spent solving in seconds. Cannot be larger than `limit_s`, but can be smaller.
- `solutions`: Solutions found during optimization. If this is 0, it's a bust!
- `variables`: Number of variables created from the problem statement. (Here, varies from 71 to 2451 depending on problem.)
- `propagators`: Number of variables created from the problem statement. (Here, varies from 78 to 2891 depending on problem.)
- `propagations`: Number of propagator invocations. (Here, maximum observed value overall is 86 million.)
- `nodes`: Number of search nodes. (Here, maximum observed value overall is 2 million.) 
- `failures`: Number of leaf nodes that were failed. (Here, maximum observed value overall is above 1 million.)
- `restarts`: Number of times the solver restarted the search (jumped back to the root search node). (Here, always 0.)
- `peak_depth`: Peak depth of search tree reached. (Here, `prepare14` reaches 1300.)
- `num_solutions`: The `nSolutions` value, this should be the "number of solutions output". It think.

## Info on search strategy annotations 

See

   - [MiniZinc manual: Search](https://www.minizinc.org/doc-2.5.5/en/mzn_search.html)
   - [MiniZinc manual: Search annotations](https://www.minizinc.org/doc-2.5.5/en/fzn-spec.html?highlight=dom_w_deg#search-annotations) (complete list of currently available search annotations)

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

### Value selection strategy

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
- [CP Profiler](https://www.minizinc.org/doc-2.5.5/en/cpprofiler.html)

## Bugs

They probably exist.




