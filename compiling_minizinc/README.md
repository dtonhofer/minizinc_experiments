# Building the MiniZinc libraries and tools

Below are some notes on how to build, on Linux:

- The "Gecode" solver libraries and tools: [Building Gecode](building_gecode.md)
- The "Chuffed" solver libraries and tools: [Building Chuffed](building_chuffed.md)
- The "libminizinc" libraries and tools: [Building libminizinc](building_libminizinc.md)

You want to first build Gecode and Chuffed.

In particular, libminizinc needs files provided from Gecode, so will be build. last.

Here is an image depicting how these elements fits together (reality may differ slightly):

![MiniZinc pipeline](minizinc_pipeline.graphml)

- The user provides a model file `model.mzn` and possible a configuration/parameter file `data.dzn`.
  These are written in the _MiniZinc_ language (a restricted/manageable form of the full _Zinc_ language).
- The `minizinc` tool provided by "libminizinc" (the latter a package of tools and libraries) compiles
  these files into an intermediary file in the _FlatZinc_ language. If it is know to what solver
  this will be given, solver-specific annotations can added.
- The model file in FlatZinc is given as input to one of several solvers able to read FlatZinc code.
  In this case we have Chuffed and Gecode.
  - Chuffed provides the tool `fzn-chuffed` to read a FlatZinc file, build a model using the 
    Chuffed classes, and solve it.
  - Gecode provides the tool `fzn-gecode` to read a FlatZinc file, build a model using the 
    Gecode classes, and solve it.
- The pipeline is managed either by the `minizinc` tool, or from the command line, or a 
  script, or through the MiniZinc IDE.




    

