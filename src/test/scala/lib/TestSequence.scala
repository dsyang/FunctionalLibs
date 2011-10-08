import org.scalacheck._
import lib.Sequences._
  import Prop._
  import Gen._


object ArraySequenceSpec extends Properties("ArraySequence") {

  val Seq = ArraySequence

  property("empty") = forAll { (n:Int) => (ArraySequence.empty).length == 0 }

  property("singleton") = forAll { (n:Int) =>
    val v = Seq.singleton(n)
    (v.length == 1) && (v(0) == n)
                                }

  property("nth") = forAll { (str: String) =>
    (Seq.singleton(str)).nth(0) == str
                            }

  property("fromList/toList") = forAll { (l:List[Int]) =>
    val s = Seq.fromList(l)
    (s.length == l.length) && (s.toList == l)
                                      }

  property("tabulate") = forAll { (n:Int) =>
    val iSeq = Seq.tabulate((i:Int) => i.toString)(n % 100)
    iSeq.length == 0 || (iSeq((n%100/2)) == (n%100/2).toString)
                               }

  property("map") = forAll { (l:List[Int]) =>
    val iSeq = Seq.fromList(l)
    (((iSeq.map((i:Int) => i.toString)).map((s:String) => s.toInt)).toList  == l)
                          }


  property("map2") = forAll {(l:List[Int], y:List[String]) =>
    val seq2 = (Seq.fromList(l)).map2((x:Int, y:String) => x.toString + y)(Seq.fromList(y))
    (seq2.length == 0 || seq2(0) == l(0).toString + y(0))
                           }

  property("reduce") = forAll { (l:List[String]) =>
    l.length == 0 || ((Seq.fromList(l).reduce((h,t) => t)("")) == l.last)
                             }

  val list_gen_no_five = listOf[Int] (Gen.choose(20,5000))

  property("filter") = forAll(list_gen_no_five)({ (l:List[Int]) =>
    val filtered = Seq.fromList(5 :: 3 :: l).filter((i:Int) => i < 10)
    (filtered.length == 2 && filtered.nth(0) == 5 && filtered.nth(1) == 3)
                                               })
}

object ParArraySequenceSpec extends Properties("ParArraySequence") {
  import Prop._

  val Seq = ParArraySequence

  property("empty") = forAll { (n:Int) => (ArraySequence.empty).length == 0 }

  property("singleton") = forAll { (n:Int) =>
    val v = Seq.singleton(n)
    (v.length == 1) && (v(0) == n)
                                }

  property("nth") = forAll { (str: String) =>
    (Seq.singleton(str)).nth(0) == str
                            }

  property("fromList/toList") = forAll { (l:List[Int]) =>
    val s = Seq.fromList(l)
    (s.length == l.length) && (s.toList == l)
                                      }

  property("tabulate") = forAll { (n:Int) =>
    val iSeq = Seq.tabulate((i:Int) => i.toString)(n % 100)
    iSeq.length == 0 || (iSeq((n%100/2)) == (n%100/2).toString)
                               }

  property("map") = forAll { (l:List[Int]) =>
    val iSeq = Seq.fromList(l)
    (((iSeq.map((i:Int) => i.toString)).map((s:String) => s.toInt)).toList  == l)
                          }

  property("map2") = forAll {(l:List[Int], y:List[String]) =>
    val seq2 = (Seq.fromList(l)).map2((x:Int, y:String) => x.toString + y)(Seq.fromList(y))
    (seq2.length == 0 || seq2(0) == l(0).toString + y(0))
                           }

  property("reduce") = forAll { (l:List[String]) =>
    l.length == 0 || ((Seq.fromList(l).reduce((h,t) => t)("")) == l.last)
                             }

  val list_gen_no_five = listOf[Int] (Gen.choose(20,5000))

  property("filter") = forAll(list_gen_no_five)({ (l:List[Int]) =>
    val filtered = Seq.fromList(5 :: 3 :: l).filter((i:Int) => i < 10)
    (filtered.length == 2 && filtered.nth(0) == 5 && filtered.nth(1) == 3)
                                               })

}