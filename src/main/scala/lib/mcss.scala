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
}

object mcss_run {
  def main(args: Array[String]) {

  }

}
