import lib.Sequences._
import System._
import scala.math

object MCSS {
  type quad = (Int, Int, Int, Int)

  def mcss(a: Sequence[Int]) : Int = {
    def mcssHelp(c: Sequence[Int]) : quad = {
      c.showt() match {
        case EMPTY() => (0,0,0,0)
        case LEAF(v) => (math.max(0,v),math.max(0,v),math.max(0,v),v)
        case NODE(a1,a2) => {
          val (b1,b2)  = (mcssHelp(a1), mcssHelp(a2))
          val (m1,p1,s1,t1) = b1
          val (m2,p2,s2,t2) = b2

          (math.max(s1+p2,math.max(m1,m2)), math.max(p1,t1+p2), math.max(s2, s1+t2), t1+t2)
        }
      }
    }
    val (b,_,_,_) : quad = mcssHelp(a)
    return b
  }

	def scan_mcss(a: Sequence[Int]) : Int = {
		val (x, t) = a.scan(_+_)(0)
    val tab = ParArraySequence.tabulate(i => if(i+1 != x.length) x(i+1)
                                             else t) (x.length)
		val (m,f) = tab.scan(math.min)(Int.MaxValue)
    val tab2 = ParArraySequence.tabulate(i => if(i+1 != m.length) m(i+1)
                                              else f) (m.length)
		val diff = ParArraySequence.tabulate (i => tab.nth(i) - tab2.nth(i))(a.length)

		diff.reduce((a,b) => math.max(a,b))(Int.MinValue)
	}


}

object mcss_run {
  import MCSS._
  def main(args: Array[String]) {
    val s1 = ParArraySequence.fromList (List(4, 8, 15, 16, 23, 42))
    val s2 = ParArraySequence.fromList (List(-5, 6, 0, 4, -5, -3, -1, 0, 7, 1))
    println("Sequence: " + scan_mcss(s1) + " Check: " + mcss(s1))
    println("Sequence: " + scan_mcss(s2) + " Check: " + mcss(s2))
  }

}
