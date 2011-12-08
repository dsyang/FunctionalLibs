import lib.Sequences._
import System._

object MCSS {
	type quad[T] = ([T], [T], [T], [T])
	
	def mcss[T](A: Sequence[T]) : T = {
		def mcss'[T](A: Sequence[T]) : quad[T] = {
			A.showt() match {
				case EMPTY => (0,0,0,0)
				case ELT(v) => (Math.max(0,v),Math.max(0,v),Math.max(0,v),v)
				case NODE(A1,A2) =>
					val (B1,B2) : (T,T) = (mcss'(A1), mcss'(A2))
					val (M1,P1,S1,T1) : quad[T] = B1
					val (M2,P2,S2,T2) : quad[T] = B2
					
					(Math.max(S1+P2,Math.max(M1,M2)), Math.max(P1,T1+P2), Math.max(S2, S1+T2), T1+T2)
			}
		}
		val (B,_,_,_) : quad[T] = mcss'(A)
		
		B
	}
}