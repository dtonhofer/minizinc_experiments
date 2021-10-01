# Building the "Chuffed" solver

## About "Chuffed"

A short description of Chuffed can be found at the [github page for Chuffed](https://github.com/chuffed/chuffed).

> **Chuffed, a lazy clause generation solver**
>
> Chuffed is a state of the art lazy clause solver designed from the ground up with
> lazy clause generation in mind. Lazy clause generation is a hybrid approach 
> to constraint solving that combines features of finite domain propagation
> and Boolean satisfiability.

There is also a CP-Profiler tool "which provides search tree visualisations for executions of constraint programming solvers" 
at [this GitHub page](https://github.com/cp-profiler/cp-profiler).

## Getting "Chuffed"

The Chuffed github repository is here: https://github.com/chuffed/chuffed

Perform either of the following on the command line (as a standard user, not `root`):

```
$ git clone git@github.com:chuffed/chuffed.git
```

which may ask for your github account password, or

```
$ git clone https://github.com/chuffed/chuffed.git

```
which needs no password.

This will create a local directory `chuffed`

```
$ tree -L 1 -F chuffed/
chuffed/
├── chuffed/
├── chuffed.msc.in
├── CMakeLists.txt
├── DESCRIPTION
├── LICENSE
├── README.md
└── submodules/
```

Then

```
$ cd chuffed/
```

List git repository branches:

```
$ git branch -a
* master
  remotes/origin/HEAD -> origin/master
  remotes/origin/Update_mznlib
  remotes/origin/develop
  remotes/origin/master
```

There is the "master" branch and the "develop" branch, the latter actually existing on the remote repository.

As indicated by the `*`, we are currently on the "master" branch:

```
$ git branch --show-current
master
```

Check the log of the currently check-out branch:

```
$ git log
commit 23b9fcee3bb30b11f68d82ef4534040ebae1a8fb (HEAD -> master, tag: 0.10.4, origin/master, origin/HEAD)
Author: Guido Tack <guido.tack@monash.edu>
Date:   Mon Jun 24 17:41:05 2019 +1000

    Fix typo
```

You _may_ want to switch to the `develop` branch to use the latest bugfixes. 
Let's try that.

```
$ git checkout develop
Branch 'develop' set up to track remote branch 'develop' from 'origin'.
Switched to a new branch 'develop'
```

```
$ git branch -a
* develop
  master
  remotes/origin/HEAD -> origin/master
  remotes/origin/Update_mznlib
  remotes/origin/develop
  remotes/origin/master
```

```
$ git log
commit 81906e8b9ac264c9caf4df588f770d0c5dcda099 (HEAD -> develop, origin/develop)
Merge: 90d58de dc0775e
Author: Guido Tack <guido.tack@monash.edu>
Date:   Mon Sep 20 14:13:57 2021 +1000

    Merge pull request #83 from aekh/bugfix
    
    Fixed issue with --no-learn flag
```

All the files should be as "freshly checked out, unmodified":

```
$ git status
On branch develop
Your branch is up to date with 'origin/develop'.

nothing to commit, working tree clean
```

## Configuring and Compiling

Your current directory should be the toplevel directory of the `chuffed` distribution.

The build process uses [_CMake_](https://en.wikipedia.org/wiki/CMake) to create the actual makefiles, so you need to have that on your system.

_CMake_'s [User Interaction Guide](https://cmake.org/cmake/help/latest/guide/user-interaction/index.html) can provide
you with a quick introduction to compiling _CMake_ projects.

Make sure you have what's needed. On my machine I have the following, but this differs between systems 
(different versions, different tools, different pacckage managers):

```
$ rpm --query cmake flex bison gcc gcc-c++
cmake-3.19.7-1.fc33.x86_64
flex-2.6.4-5.fc33.x86_64
bison-3.6.4-3.fc33.x86_64
gcc-10.3.1-1.fc33.x86_64
gcc-c++-10.3.1-1.fc33.x86_64
```

The build process is run by executing the following in turn. We will install the (development version of)
Chuffed to directoy `/usr/local/minizinc/chuffed_2021_09_20` and create a "debug" build. Use `time`
to evaluate the time taken to compile. On my machine, it takes a bit more than 1 minute.

```
$ mkdir build 
$ cd build
$ cmake -DCMAKE_INSTALL_PREFIX=/usr/local/minizinc/chuffed_2021_09_20 -DCMAKE_BUILD_TYPE=Debug ..
$ time cmake --build . --verbose
```

### Errors may appear if you are compiling the `master` branch

At this point, errors may appear if you are compiling the `master` branch instead of the `development` branch.
See this [StackOverflow question](https://stackoverflow.com/questions/68655384/compilation-by-g-of-parser-code-created-by-bison-fails-because-yytokentype-e)

Someone suggest a problem with `parser.tab`. Underneath the toplevel "chuffed" directory, there is:

```
./chuffed/flatzinc/parser.tab.cpp
./chuffed/flatzinc/parser.tab.h
```

These should not be there! Remove them.

Then remove the "build" directory and start again from the point where you created it.

## Installing

Switch to user `root`, change to the `build` directory in which you just compiled, then:

```
# cmake --build . --target install
```

Then we get the following (here, abbreviated) filetree in /usr/local/minizinc/chuffed_2021_09_20/`:

```
/usr/local/minizinc/chuffed_2021_09_20/
├── bin
│   └── fzn-chuffed
├── chuffed.msc
└── share
    └── chuffed
        └── mznlib
            ├── all_different_int.mzn
            ├── arg_max_bool.mzn
            ├── at_least_int.mzn
            ├── at_most_int.mzn
            ├── chuffed.mzn
            ├── circuit.mzn
            ├── cost_regular.mzn
            ├── count.mzn
            ├── cumulative.mzn
            ├── disjunctive.mzn
            ├── disjunctive_strict.mzn
            ├── distribute.mzn
            ├── exactly_int.mzn
            ├── global_cardinality_low_up.mzn
            ├── inverse.mzn
            ├── maximum_int.mzn
            ├── minimum_int.mzn
            ├── nvalue.mzn
            ├── redefinitions.mzn
            ├── regular.mzn
            ├── subcircuit.mzn
            ├── table_int.mzn
            ├── values_interchange.mzn
            ├── values_sequences.mzn
            ├── variables_interchange.mzn
            └── variables_sequences.mzn
```

`fzn-chuffed` is the "chuffed flatzinc executable", which reads FlatZinc language files and solves them.

For easy accessibility, create a symlink:

```
# cd /usr/local/minizinc
# ln -s chuffed_2021_09_20/ chuffed
```

## Testing

There seems to be no tests.

But we can build the examples. As non-root, go back to the `build` directory and do:

```
$ cmake --build . --target examples
```

This compilation currently fails on the `develop` branch

```
[ 61%] Building CXX object CMakeFiles/nurse.dir/chuffed/examples/nurse.cpp.o
nurse.cpp:18:10: fatal error: mdd/circ_fns.h: No such file or directory
   18 | #include <mdd/circ_fns.h>
      |          ^~

```

It works on the `master` branch. A series of executables appear in directory build.
However, there is no info on how to run those.

Indeed the code of the examples is so minimal that you have to pass the correct arguments.
If you don't they will segfault (for good reason as they try to get unavailable arguments
out of `argv`).




