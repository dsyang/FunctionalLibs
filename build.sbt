//sbt build project
name := "FuncionalLibs"

//version
version := "0.1"


libraryDependencies += "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test"

// reduce the maximum number of errors shown by the Scala compiler
maxErrors := 20

// increase the time between polling for file changes when using continuous execution
pollInterval := 1000

// append -deprecation to the options passed to the Scala compiler
scalacOptions ++= Seq("-unchecked", "-deprecation")

// define the statements initially evaluated when entering 'console', 'console-quick', or 'console-project'
initialCommands := """
  import System.{currentTimeMillis => now}
  def time[T](f: => T): T = {
    val start = now
    try { f } finally { println("Elapsed: " + (now - start)/1000.0 + " s") }
  }
"""
