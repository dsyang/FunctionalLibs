# FunctionalLibs: A library of collections in Scala

An experimental port of the 15-210 library in Scala.

**Why?**
To take advantage of Scala's Parallel Collections and actually run parallel algorithms in parallel.

**Does it actually work?**
When there is a good amount of computation done, the performance increase is obvious (as seen in the included demo).  When the computation is not as extensive, the difference is not as noticeable. In fact, the overhead of creating and joining threads may actually make the parallel implementation less efficient.


Running the included demo

    sbt> run
    [info] Running Demo.Experiment
    Doing a map on lists of length 100 where the operation sleeps for 10ms
    Length of list can be passed as argument. i.e. sbt> run 1000
    Sequential 1026(ms)
    Parallel 269(ms)


Running the Stock Market Problem from one of the 15-210 HWs

    sbt> run
    [info] Running StocksMarketProblem
    Running Sequentially
    Running in Parallel
    Sequential: totals= 32,18,17,23,27(ms)
    Parallel: totals= 22,23,22,20,23(ms)


**Running the demo**

+ get scala 2.9.1 and sbt 0.11.  If you're on a mac, [homebrew](http://mxcl.github.com/homebrew/) is a great tool for installing packages. Just run ```brew install scala``` and ```brew install sbt```

+ clone the repo. then at repo root, run ```sbt```

+ Some useful sbt commands include ```run```, ```compile```, ```test```, and ```clean```.  These should be self-explanatory

