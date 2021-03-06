# Building "libminizinc", the MiniZinc compiler and libraries

The manual has some description on how to build "libminizinc" on Linux:

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

We assume that we have already compiled the solver libraries "Gecode" and "Chuffed"at this point,
so we can use Gecode during compilation of libminizinc.

The installation uses [CMake](https://en.wikipedia.org/wiki/CMake) to create the actual makefiles, so you need to have that on your system.

CMake's [User Interaction Guide](https://cmake.org/cmake/help/latest/guide/user-interaction/index.html) can provide
you with a quick introduction to compiling _CMake_ projects.

Make sure you have what's needed. Commands differ between systems:

```
$ rpm --query cmake bison flex gcc-c++
cmake-3.19.7-1.fc33.x86_64
bison-3.6.4-3.fc33.x86_64
flex-2.6.4-5.fc33.x86_64
gcc-c++-10.3.1-1.fc33.x86_64
```

## Getting libminizinc

The github repository is here: https://github.com/MiniZinc/libminizinc

Perform either of the following on the command line:

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
$ tree -L 1 -F libminizinc/

libminizinc/
????????? changes.rst
????????? cmake/
????????? CMakeLists.txt
????????? docs/
????????? include/
????????? lib/
????????? LICENSE.txt
????????? minizinc.cpp
????????? mzn2doc.cpp
????????? README.md
????????? share/
????????? solvers/
????????? tests/
```

Then:

```
$ cd libminizinc/
```

List git repository branches:

```
$ git branch -a
* master
  remotes/origin/HEAD -> origin/master
  remotes/origin/develop
  remotes/origin/master
```

There is the `master` branch and the `develop` branch, the latter on the remote repository. 

As indicated by the `*`, we are currently on the `master` branch:

```
$ git branch --show-current
master
```

Yes, master!

If you want to switch branch to `develop` to get latest fixes:

```
$ git checkout develop
Branch 'develop' set up to track remote branch 'develop' from 'origin'.
Switched to a new branch 'develop'
```

Switching to `develop` may be advantageous as it comes ith bug fixes that are not yet in the "relaease".

Check the log of the currently checked-out branch:

```
$ git log
commit 0848ce7ec78d3051cbe0f9e558af3c9dcfe65606 (HEAD -> master, tag: 2.5.5, origin/master, origin/HEAD)
Author: Jason N <admin@cyderize.org>
Date:   Fri Mar 19 14:11:01 2021 +1100

    Add changelog for 2.5.5
```

If you are on the `develop` branch, the messages are much fresher:

```
$ git log
commit 175d055119f7d8e8c0d0972774824124230646c1 (HEAD -> develop, origin/develop)
Author: Jip J. Dekker <jip@dekker.one>
Date:   Fri Oct 1 17:42:54 2021 +1000

    Fix lost output in simplify_constraint for functional _eq constraints
    
    (Seems this case was missing, it was present in the _le case)
    
    Fixes #503
```

Check whether any files have changed. In the example below, I have switched to branch `develop` beforehand:

```
$ git status
On branch develop
Your branch is up to date with 'origin/develop'.

nothing to commit, working tree clean
```

`origin/develop` is the remote branch (branch `develop` on the remote `origin`), branch `develop` is a local branch.
And we are up-to-date with the remote. Good!

## Configuring and compiling

The [README.md]( https://github.com/MiniZinc/libminizinc/blob/master/README.md) says:

```
The following CMake variables can be used in the MiniZinc project to instruct the
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

We have to hint at the location of Gecode, compiled previously! 
In our case, we put it into `/usr/local/minizinc/gecode/`, and we will use that path below.

The manual says:

> After installing Gecode, you can compile MiniZinc with built-in support for Gecode, which enables 
> extended pre-solving and solution checking. In order for the MiniZinc compilation to find your Gecode
> installation, Gecode either has to be in the default location (such as /usr/local/include etc.), or you
> have to use the option -DGECODE_ROOT= when calling cmake.

As said, we have `gecode` in `$MZN/gecode/` and want `libminizinc` to go to `$MZN/libminizinc_${VERSION}`.

```
$ cd libminizinc    # ... you should already be in that directory
$ mkdir build
$ cd build
$ MZN=/usr/local/minizinc
$ VERSION=2021_10_01
$ export Gecode_ROOT=$MZN/gecode/
$ cmake -DCMAKE_INSTALL_PREFIX=$MZN/libminizinc_${VERSION} -DCMAKE_BUILD_TYPE=Release ..
```

Look for the message

```
-- Found Gecode: /usr/local/minizinc/gecode/include (found suitable version "6.3.1", minimum required is "6.0") 
found components: Driver Float Int Kernel Minimodel Search Set Support
```

Note the output, check it for interesting messages. 

Then compile. Use `time` to see how long that takes:

```
$ time cmake --build .
```

After about 5 minutes you see:

```
[ 91%] Building CXX object CMakeFiles/mzn.dir/lib/type.cpp.o
[ 92%] Building CXX object CMakeFiles/mzn.dir/lib/typecheck.cpp.o
[ 93%] Building CXX object CMakeFiles/mzn.dir/lib/values.cpp.o
[ 95%] Linking CXX static library libmzn.a
[ 95%] Built target mzn
Scanning dependencies of target minizinc
[ 96%] Building CXX object CMakeFiles/minizinc.dir/minizinc.cpp.o
[ 97%] Linking CXX executable minizinc
[ 97%] Built target minizinc
Scanning dependencies of target mzn2doc
[ 98%] Building CXX object CMakeFiles/mzn2doc.dir/mzn2doc.cpp.o
[100%] Linking CXX executable mzn2doc
[100%] Built target mzn2doc
```

## Installing

As `root`, cd to the `build` directory and then run:

```
# cmake --build . --target install
```

A tree appears at the configured location:

```
# tree -F -L 2 /usr/local/minizinc/libminizinc_2021_10_01/
/usr/local/minizinc/libminizinc_2021_10_01/
????????? bin/
??????? ????????? minizinc*
??????? ????????? mzn2doc*
????????? include/
??????? ????????? minizinc/
????????? lib64/
??????? ????????? cmake/
??????? ????????? libmzn.a
????????? share/
    ????????? minizinc/
```

Create a symlink to make `libminizinc` an alias to `libminizinc_2021_10_01`:

```
$ cd /usr/local/minizinc/
$ ln -s libminizinc_2021_10_01/ libminizinc
```

Extend the `PATH` as configured in `.bashrc` so that the `minizinc` executable can be found. In my case:

```
PATH="$PATH:/usr/local/minizinc/libminizinc/bin"
```

## Verify

Let's see the solvers that minizinc knows about!

Drop root privileges, then:

```
$  minizinc --help
minizinc: error while loading shared libraries: libgecodedriver.so.52: cannot open shared object file: No such file or directory
```

Evidently the Gecode libraries are not being found.

This can be solved by extending `LD_LIBRARY_PATH` (but this is just a temporary solution)

```
$ export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/minizinc/gecode/lib/
```

After that

```
$  minizinc --help
```

works. For a permanent solution, switch to `root`, then:

Add the file `gecode.conf` to directory `/etc/ld.so.conf.d`. 
That file simply contains the name of the directory with the `gecode` library files.
One can even use a symlink. The content is thus simply:

```
/usr/local/minizinc/gecode/lib/
```

After that file has been created, rebuild the "loader" cache by running:

```
# ldconfig
```

## Making Gecode visible to libminizinc

The gecode solver is not yet visible!

```
$  minizinc --solver gecode
Config exception: no solver with tag gecode found
minizinc: MiniZinc driver.
Usage: minizinc  [<options>] [-I <include path>] <model>.mzn [<data>.dzn ...] or just <flat>.fzn
More info with "minizinc --help"
```

The manual says:

> In order to use Gecode as a solver with MiniZinc (as opposed to an internal
> pre-solving tool), you have to create an appropriate solver configuration file.
>
> Add a file gecode.msc in an appropriate location (see [Solver Configuration Files](https://www.minizinc.org/doc-2.5.5/en/fzn-spec.html#sec-cmdline-conffiles)) 
> containing the following, where you replace with the actual installation path and
> update the version number if necessary:

So here is the updated JSON file, nominally for "release 6.2.0" although we are working with the 

```
{
 "id": "org.gecode.gecode",
 "name": "Gecode",
 "description": "Gecode FlatZinc executable",
 "version": "6.2.0",
 "mznlib": "/usr/local/minizinc/gecode/share/gecode/mznlib/",
 "executable": "/usr/local/minizinc/gecode/bin/fzn-gecode",
 "tags": ["cp","int", "float", "set", "restart"],
 "stdFlags": ["-a","-f","-n","-p","-r","-s","-t"],
 "supportsMzn": false,
 "supportsFzn": true,
 "needsSolns2Out": true,
 "needsMznExecutable": false,
 "needsStdlibDir": false,
 "isGUIApplication": false
}
```

The above file goes to directory (MZN is our MiniZinc installation directory):

```
MZN=/usr/local/minizinc
$MZN/libminizinc/share/minizinc/solvers
```

N.B. It is under directory `share` **not** under `include`.

The directory may not exist at first. Thus:

```
$ MZN=/usr/local/minizinc
$ mkdir $MZN/libminizinc/share/minizinc/solvers
```

Now add the above JSON as a file `gecode.msc`.

Once that has been done, `gecode` is visible to `minizinc`:

``` 
$ minizinc --solvers
MiniZinc driver.
Available solver configurations:
  Gecode 6.2.0 (org.gecode.gecode, default solver, cp, int, float, set, restart)
Search path for solver configurations:
  /usr/local/minizinc/libminizinc/share/minizinc/solvers
  /usr/local/share/minizinc/solvers
  /usr/share/minizinc/solvers
``` 

Here is a remark that I did not have to act on, but good to know:

> You may have to add <YOUR_SOLVER_INSTALL_PREFIX>/share/minizinc/solvers/ 
> to the MZN_SOLVER_PATH environment variable if the solvers haven't been 
> installed in the same place as MiniZinc."

## Making Chuffed visible to libminizinc

There is a `chuffed.msc` file in the chuffed installation, nominally for "release 6.2.0" although we are working with the `develop` branch:

```
/usr/local/minizinc/chuffed/chuffed.msc
```

Just switch to `root` and copy that file into the `libminizinc` filetree:

```
# MZN=/usr/local/minizinc
# cd $MZN/libminizinc/share/minizinc/solvers
# cp $MZN/chuffed/chuffed.msc .
```

Now `libminizinc` knows about the `chuffed` solver:

```
$ minizinc --solvers
MiniZinc driver.
Available solver configurations:
  Chuffed 0.10.4 (org.chuffed.chuffed, cp, lcg, int)
  Gecode 6.2.0 (org.gecode.gecode, default solver, cp, int, float, set, restart)
Search path for solver configurations:
  /usr/local/minizinc/libminizinc/share/minizinc/solvers
  /usr/local/share/minizinc/solvers
  /usr/share/minizinc/solvers
```

**NOTE** The `chuffed.msc` files differ substantially between the `master`/release version and the `develop` branch version!
In particular, it lists all the "extra" flags.

## Running tests

See https://github.com/MiniZinc/libminizinc/tree/master/tests

> The correctness of the MiniZinc compiler is tested using a PyTest test suite.

Make sure you aren't `root` when running the tests.
 
First, cd to the `tests` subdirectory of the distribution (i.e. not under `build`):

```
$ cd tests
```

Get the Python stuff you need:

```
$ pip install -r requirements.txt
```

This will install required Python packages. Note that the `master`/release version needs `PyYAML 5.3` but the `devel`
version needs `PyYAML 5.4`.

Now run the tests, indicating the just installed version as "driver":

```
$ pytest --solvers gecode,chuffed --driver /usr/local/minizinc/libminizinc/bin
```

With the "master" branch, this actually terminates mostly in Python errors of this kind:

```
TypeError: argument of type 'NoneType' is not iterable
```

With the "develop" branch, thinks are looking a bit up:

```
14 failed, 401 passed, 608 skipped, 4 warnings in 90.66s (0:01:30)
```

This needs some work!

On the other hand, the `develop` branch fails because it tries to create a cache where it can't:

```
PytestCacheWarning: could not create cache path /usr/local/minizinc/libminizinc/bin/.pytest_cache/v/cache/nodeids
```

