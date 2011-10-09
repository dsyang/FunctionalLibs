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


Running the Stock Market Problem from HW on actual stocks:

    sbt> run
    [info] Running RunStocks
    Running test.Some(10.0)  start: 1 | end: 7
    Running on GOOG weekly prices from 2004-2010:  The jump: Some(614.86)  Buy on:2004-08-30 Sell at: 2007-12-03
            seq time: 8(ms)
            par time: 10(ms)
    Running on ARMH weekly prices from 2000-2010:  The jump: Some(15.77)  Buy on:2000-01-18 Sell at: 2000-02-28
            seq time: 20(ms)
            par time: 18(ms)
    Running on AAPL weekly prices from 1990-2010:  The jump: Some(208.64999999999998)  Buy on:1997-12-22 Sell at:  2010-01-04
            seq time: 47(ms)
            par time: 30(ms)

**Running the demo**

+ get scala 2.9.1 and sbt 0.11.  If you're on a mac, [homebrew](http://mxcl.github.com/homebrew/) is a great tool for installing packages. Just run ```brew install scala``` and ```brew install sbt```

+ clone the repo. then at repo root, run ```sbt```

+ Some useful sbt commands include ```run```, ```compile```, ```test```, and ```clean```.  These should be self-explanatory

