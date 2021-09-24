# Stats Collection

Collecting statistics over several runs of MiniZinc solving a scheduling problem.

This problem is from week 1 of the course [Solving Algorithms for Discrete Optimization](https://www.coursera.org/learn/solving-algorithms-discrete-optimization)
by Prof. Jimmy Ho Man Lee and Prof. Peter James Stuckey.

The idea is to tune the variable selection and value selection hints and take some
conclusion as to what's best.

But why do it manually?

We write a Perl program that runs MiniZinc with a slightly modified version of the model.
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

## Info on search strategy annotations 

See

   - [MiniZinc manual: Search](https://www.minizinc.org/doc-2.5.5/en/mzn_search.html)
   - [MiniZinc manual: Search annotations](https://www.minizinc.org/doc-2.5.5/en/fzn-spec.html?highlight=dom_w_deg#search-annotations) (complete list of currently available search annotations)

### Variable selection strategy

- *input_order*: choose in order from the array
- *first_fail*: choose the variable with the smallest domain size
- *anti_first_fail*: choose the variable with the largest domain
- *smallest*: choose the variable with the smallest value in its domain
- *largest*: choose the variable with the largest value in its domain
- *occurrence*: choose the variable with the largest number of attached constraints
- *most_constrained*: choose the variable with the smallest domain, breaking ties using the number of constraints
- *max_regret*: choose the variable with the largest difference between the two smallest values in its domain.
- *dom_w_deg*: choose the variable with the smallest value of domain size divided by weighted degree, which is the number of times it has been in a constraint that caused failure earlier in the search

### Value selection strategy

- *indomain_min*: assign the smallest value in the variable's domain
- *indomain_max*: assign the largest value in the variable's domain
- *indomain_middle*: assign the value in the variable’s domain closest to the mean of its current bounds
- *indomain_median*: assign the middle value in the variable’s domain
- *indomain*: nondeterministically assign values to the variable in ascending order
- *indomain_random*: assign a random value from the variable’s domain
- *indomain_split*: bisect the variable’s domain, excluding the upper half first.
- *indomain_reverse_split*: bisect the variable’s domain, excluding the lower half first.
- *indomain_interval*: if the variable’s domain consists of several contiguous intervals, reduce the domain to the first interval. Otherwise just split the variable’s domain.

## Bugs

They probably exist.







