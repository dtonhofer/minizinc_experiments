# Building the MiniZinc compiler (`libminizinc) itself 

The manual has some description on how to build `libminizinc` on Linux:

https://www.minizinc.org/doc-2.5.5/en/installation_detailed_linux.html

> Before compiling MiniZinc, consider whether you want to install additional
> solvers. In particular, MIP solvers such as CBC, SCIP, Gurobi, IBM ILOG CPLEX, 
> and XPRESS need to be installed prior to compiling MiniZinc, so that the build 
> system can find them. If you want to use the solution checking functionality or 
> extended presolving, you will also need to install Gecode before compiling MiniZinc.

> To install MiniZinc, unpack the source code (or clone the git repository).
> Open a terminal and change into the source code directory. The following sequence of
> commands will build the MiniZinc compiler tool chain: ...

The `libminizinc` project's [README.md]( https://github.com/MiniZinc/libminizinc/blob/master/README.md) file contains building instructions.

In that README, MiniZinc team recommends you use the binary packages here: https://www.minizinc.org/software.html

**But we want to build it ourselves!**

Note that we have compiled `gecode` and `chuffed` already at this point, so we can use `gecode` during compilation of `libminizinc`.

## Getting _libminizinc_

It is here: https://github.com/MiniZinc/libminizinc

Get it with the git protocol. Perform either of the following on the command line:

```
git clone git@github.com:MiniZinc/libminizinc.git
```

which may ask for your github account password, or

```
git clone https://github.com/MiniZinc/libminizinc.git
```

which needs no password.

This will create a local directory `libmimizinc`

```
$ tree -L 1 libminizinc/

libminizinc/
├── changes.rst
├── cmake
├── CMakeLists.txt
├── docs
├── include
├── lib
├── LICENSE.txt
├── minizinc.cpp
├── mzn2doc.cpp
├── README.md
├── share
├── solvers
└── tests
```

Thus

```
cd libminizinc
```

List git repository branches

```
$ git branch -a
* master
  remotes/origin/HEAD -> origin/master
  remotes/origin/develop
  remotes/origin/master
```

There is the "master" branch and the "develop" branch, the latter is on the remote. 

What is the currently checked-out branch?

```
$ git branch --show-current
master
```

Yes, master!

Switch branch to `develop` to get latest fixes (but you probably want to skip this to stay on `master`):

```
$ git checkout develop
Branch 'develop' set up to track remote branch 'develop' from 'origin'.
Switched to a new branch 'develop'
```

Check the log of the currently check-out branch:

```
$ git log
commit bbefcea214fec798a0f5acc442581984555acd21 (HEAD -> develop, origin/develop)
Author: Mikael Zayenz Lagerkvist <lagerkvist@gecode.org>
Date:   Fri May 14 16:13:20 2021 +0200
 
    Fix FlatZinc posting for element constraints
```

Check whether any files have changed. In the example below, I have switched to branch `develop` beforehand:

```
$ git status
On branch develop
Your branch is up to date with 'origin/develop'.
```

`origin/develop` is the remote branch (branch `develop` on the remote `origin`), `develop` is a local branch.

## Compiling

The installation uses [_CMake_](https://en.wikipedia.org/wiki/CMake) to create the actual makefiles, so you need to have that on your system.

The [README.md]( https://github.com/MiniZinc/libminizinc/blob/master/README.md) says:

```
The MiniZinc compiler is compiled as a CMake project. CMake's [User Interaction
Guide](https://cmake.org/cmake/help/latest/guide/user-interaction/index.html)
can provide you with a quick introduction to compiling CMake projects. The
following CMake variables can be used in the MiniZinc project to instruct the
compilation behaviour:
 
| Variable                                     | Default | Description                                                 |
|----------------------------------------------|---------|-------------------------------------------------------------|
| CMAKE_BUILD_TYPE                             | Release | Build type of single-configuration generators.              |
| CMAKE_INSTALL_PREFIX                         |         | Install directory used by `--target install`.               |
| CMAKE_POSITION_INDEPENDENT_CODE              | TRUE    | Whether to create a position-independent targets            |
| **<solver_name>**_ROOT                       |         | Additional directory to look for **<solver_name>**          |
| CMAKE_DISABLE_FIND_PACKAGE_**<solver_name>** | FALSE   | Disable compilation of **<solver_name>**'s solver interface |
| USE_PROPRIETARY                              | FALSE   | Allow static linking of proprietary solvers                 |
| **<Gurobi/CPlex>**_PLUGIN                    | TRUE    | Load solver at runtime (instead of static compilation)      |
 
Possible values for **<solver_name>** are `CPlex`, `Geas`, `Gecode`, `Gurobi`, `OsiCBC`, `SCIP`, and `Xpress`.
```

You have to hint at the location of `gecode`, which you must have compiled previously! 
In our case, we put it into `/usr/local/minizinc/gecode/`, and we will use that path below.

The manual says:

> After installing Gecode, you can compile MiniZinc with built-in support for Gecode, which enables 
> extended pre-solving and solution checking. In order for the MiniZinc compilation to find your Gecode
> installation, Gecode either has to be in the default location (such as /usr/local/include etc.), or you
> have to use the option -DGECODE_ROOT= when calling cmake.

```
$ cd libminizinc # you should already be in that directory
$ mkdir build
$ cd build
$ export Gecode_ROOT=/usr/local/minizinc/gecode/
$ cmake -DCMAKE_INSTALL_PREFIX=/usr/local/minizinc/libminizinc -DCMAKE_BUILD_TYPE=Release ..
```

Note the output, check it for interesting messages.

Now compile:

```
$ cmake --build .
```

