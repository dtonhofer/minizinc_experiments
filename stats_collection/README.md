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

## Bugs

They probabyl exist.







