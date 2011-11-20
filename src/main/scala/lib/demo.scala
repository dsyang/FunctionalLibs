package Demo

import lib.Sequences._
import scala.util.Random
import System._

object Data {
  def reg(n:Int):Sequence[Data] = {
    val r = new Random
    val lists = for { i <- 0 to n } yield new Data(r.nextInt(10000), r.nextInt(10000))
    ArraySequence.fromList((lists.toList))
  }

  def par(n:Int):Sequence[Data] = {
    val r = new Random
    val lists = for { i <- 0 to n } yield new Data(r.nextInt(10000), r.nextInt(10000))
    ParArraySequence.fromList((lists.toList))
  }
  def parI(n:Int):Sequence[Int] = {
    val r = new Random
    val e = 2*n
    val lists = for {i <- 0 to e} yield r.nextInt(10000)
    ParArraySequence.fromList((lists.toList))
  }
  def regI(n:Int):Sequence[Int] = {
    val r = new Random
    val e = 2*n
    val lists = for {i <- 0 to e} yield r.nextInt(10000)
    ArraySequence.fromList((lists.toList))
  }

}


class Data(val a:Int = 0 , val b:Int = 0)

trait Computation {
  def f(data: Data):Int = {
    Thread.sleep(10)
    data.a + data.b
  }

  def compute( seq: Sequence[Data]): Int
}


object Sequential extends Computation {
  def compute(seq: Sequence[Data]):Int = seq.map(f).nth(0)
}
object Parallel extends Computation {
  def compute(seq: Sequence[Data]):Int = seq.map(f).nth(0)
}

trait ReduceComp {
  def f(i:Int, j:Int):Int = {
    Thread.sleep(10)
    i+j
  }

  def compute(seq: Sequence[Int]):Int
}

object SeqGenReduce extends ReduceComp {
  def compute(seq: Sequence[Int]):Int = seq.gen_reduce(f)
}
object ParGenReduce extends ReduceComp {
  def compute(seq: Sequence[Int]):Int = seq.gen_reduce(f)
}
object SeqReduce extends ReduceComp {
  def compute(seq: Sequence[Int]):Int = seq.reduce(f)(0)
}
object ParReduce extends ReduceComp {
  def compute(seq: Sequence[Int]):Int = seq.reduce(f)(0)
}


object Experiement {
  def main(args: Array[String]) {
    var len = 0;
    if(args.length == 0) len = 100
    else len = args(0).toInt

    val list = Data.reg(len)
    val plist = Data.par(len)
    println("Doing a map on lists of length " + len + " where the operation sleeps for 10ms")
    println("Length of list can be passed as argument. i.e. sbt> run 1000")
    var t1 = currentTimeMillis()
    Sequential.compute(list)
    var t2 = currentTimeMillis()
    println("Sequential " + (t2-t1) + "(ms)")
    t1 = currentTimeMillis()
    Parallel.compute(plist)
    t2 = currentTimeMillis()
    println("Parallell " + (t2-t1) + "(ms)")

    val sred = Data.regI(100)
    val pred = Data.parI(100)

    t1 = currentTimeMillis()
    SeqGenReduce.compute(sred)
    t2 = currentTimeMillis()
    println("Sequential Builtin Reduce: " + (t2-t1) + "(ms)")
    t1 = currentTimeMillis()
    ParGenReduce.compute(pred)
    t2 = currentTimeMillis()
    println("Parallell Builtin Reduce: " + (t2-t1) + "(ms)")

    t1 = currentTimeMillis()
    SeqReduce.compute(sred)
    t2 = currentTimeMillis()
    println("Sequential 210  Reduce: " + (t2-t1) + "(ms)")
    t1 = currentTimeMillis()
    ParReduce.compute(pred)
    t2 = currentTimeMillis()
    println("Parallell 210 Reduce: " + (t2-t1) + "(ms)")

  }
}
