# Building the MiniZinc libraries and tools + associated solvers

While the **official documentation** about building MiniZinc from source can be found in the MiniZinc manual under
[Installation from Source Code](https://www.minizinc.org/doc-2.5.5/en/installation_detailed.html), this
directory contains my notes on how to build:

- The "Gecode" solver libraries and tools: [Building Gecode](building_gecode.md)
- The "Chuffed" solver libraries and tools: [Building Chuffed](building_chuffed.md)
- The "libminizinc" libraries and tools: [Building libminizinc](building_libminizinc.md)

on Linux (in this case, Linux Fedora 33).

You want to first build Gecode as libminizinc needs files provided by Gecode during its build process.

Here is an image depicting how these elements fits together (reality may differ slightly):

![MiniZinc pipeline](minizinc_pipeline.png)

- The user provides a model file `model.mzn` and possible a configuration/parameter file `data.dzn`.
  These are written in the _MiniZinc_ language (a restricted form of the full _Zinc_ language, which may not exist yet).
- The `minizinc` tool/compiler provided by "libminizinc" (the latter a package of tools and libraries) compiles
  the _MiniZinc_ model into an intermediary model expressed in the _FlatZinc_ language. _FlatZinc_ is
  simpler so that constraint propagation solvers (or even SAT solvers) can be given a preprocessing element that
  is able to transform _FlatZinc_ models into their respective internal models (implementation language-dependent
  datastructures). The intermediary model may contain solver-specific annotations if the target solver is known.
- The model expressed in _FlatZinc_ is given to one of several solvers able to read _FlatZinc_ code.
  (It may be that the intermediary model it is not even serialized out into a file, but passed directly
  as an Abstract Syntax Tree?). In this case, Chuffed and Gecode.
  - Chuffed provides the tool `fzn-chuffed` to read a _FlatZinc_ file, build a model using the 
    Chuffed classes (C++ classes), and solve it.
  - Gecode provides the tool `fzn-gecode` to read a _FlatZinc_ file, build a model using the 
    Gecode classes (also C++ classes), and solve it.
- The pipeline is managed either by the `minizinc` tool, or from the command line, or a 
  script, or through the "MiniZinc IDE".
  
The invocation to make the `minizinc` compiler generate _FlatZinc_ output without
passing it to a solver is apparently `minizinc -c --solver org.minizinc.mzn-fzn`.

See ["Chapter 2.8: FlatZinc and Flattening"](https://www.minizinc.org/doc-2.5.5/en/flattening.html) 
in the "MiniZinc Handbook" for more on this.

See also the [Gecode documentation on _FlatZinc_](https://www.gecode.org/flatzinc.html) 
