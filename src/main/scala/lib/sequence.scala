package lib.Sequences

  import lib.utils._
  import compares._

  import scala.collection.parallel.immutable.ParVector


  //functions that act on a defined sequence
  trait Sequence[T] {
    val length: Int

    val toList: List[T]

    def apply(i:Int) : T

    def nth(i:Int) : T

    def map[V](f: (T) => V) : Sequence[V]

    def map2[A,B](f: ((T),A) => B)(b: Sequence[A]):  Sequence[B]

    def filter(f: (T) => Boolean) : Sequence[T]

    def take(i:Int) : Sequence[T]

    def drop(i:Int) : Sequence[T]

    def reduce(f: (T,T) => T)(base: T): T

    def gen_reduce(f: (T,T) => T):T

    def showl () : ListView[T]

    def showt () : TreeView[T]

  }

  // Functions that create sequences
  trait SequenceObject {
    def empty[T] : Sequence[T]

    def singleton[T](a:T) : Sequence[T]

    def fromList[T](l:List[T]) : Sequence[T]

    def tabulate[T](f: Int => T)(size: Int) : Sequence[T]

    def hidel[T](lv: ListView[T]) : Sequence[T]

    def hidet[T](t :TreeView[T]) : Sequence[T]

    def seqEQ[T](f: (T,T) => Boolean)(a: Sequence[T], b:Sequence[T]) : Boolean =
      a.map2(f)(b).reduce((b1, b2) => b1 && b2)(true)
  }

  sealed abstract class TreeView[T]
  case class EMPTY[T]() extends TreeView[T]
  case class LEAF[T]( x: T) extends TreeView[T]
  case class NODE[T](l: Sequence[T], r: Sequence[T])  extends TreeView[T]


  sealed abstract class ListView[T]
  case class NIL[T]() extends ListView[T]
  case class CONS[T](x : T, xs: Sequence[T]) extends ListView[T]


// Sequential Implementation of Sequence
  object ArraySequence extends SequenceObject {

    def empty[T] : Sequence[T] = new ArraySequenceImpl[T] (Vector.empty)
    def singleton[T](a:T) : Sequence[T] = new ArraySequenceImpl[T] (Vector(a))

    def fromList[T](l:List[T]) : Sequence[T] = new ArraySequenceImpl[T] (Vector.tabulate
                                                         (l.length)
                                                         ((n:Int) => l(n)))

    def tabulate[T](f: Int => T) (size: Int) : Sequence[T] = {
      if (size == 0) return empty
      new ArraySequenceImpl(Vector.tabulate (size) (f))
    }

    def hidel[T] (lv: ListView[T]) = lv match {
      case NIL() => ArraySequence.empty
      case CONS(h,t) => ArraySequence.tabulate(i => (if (i == 0) h
                                                     else t(i-1) ))(t.length+1)
    }

    def hidet[T] (tv: TreeView[T]) = tv match {
      case EMPTY() => ArraySequence.empty
      case LEAF(x) => ArraySequence.singleton(x)
      case NODE(l,r) => ArraySequence.tabulate( i => (if (i < l.length) l(i)
                                                      else r(i-l.length)))(l.length+r.length)
    }

    private class ArraySequenceImpl[T] (private val elems: Vector[T]) extends Sequence[T] {

      val length = elems.length

      def apply (i: Int) = elems(i)

      def nth (i:Int) = elems(i)

      def take (i:Int) = new ArraySequenceImpl[T] (elems.take(i))

      def drop (i:Int) = new ArraySequenceImpl[T] (elems.drop(i))

      val toList = elems.toList

      def gen_reduce(f: (T,T) => T):T = elems.reduce[T](f)

      def showl () : ListView[T] = elems.length match{
        case 0 => NIL()
        case _ => CONS (elems.head, ArraySequence.tabulate ((i:Int) => elems(i+1))(length-1))
      }

      def showt (): TreeView[T] = elems.length match{
        case 0 => EMPTY()
        case 1 => LEAF(elems(0))
        case _ => {val n = (elems.length / 2);NODE(this.take(n), this.drop(n))}
      }

      def map[V](f: (T) => V) = new ArraySequenceImpl[V]( elems.map(f))

      def map2[A,B] (f: ((T),A) => B) (b: Sequence[A]): Sequence[B] =
        ArraySequence.tabulate ((i:Int) => f( elems(i), b.nth(i))) ((b.length).min(elems.length))

      def filter(p:(T) => Boolean ): Sequence[T] = new ArraySequenceImpl[T](elems.filter(p))

      def reduce (f: (T,T) => T)(base:T):T =
        elems.length match{
          case 0 => base
          case _ => {
            def ppt(x:Int) = {
              def ppth(curr:Int, prev:Int):Int = {
                if(curr < x) ppth(2*curr, curr)
                else prev
              }
              ppth(1,0)
            }
            def l(a:Int, b:Int) = (a, (a+ppt((b-a)+1))-1)
            def r(a:Int, b:Int) = (a+ppt((b-a)+1), b)

            def redh(i:Int, j:Int ) : T = {
              (j-i) match {
                case 0 => elems(i)
                case _ => {
                  val (li, lj) = l (i,j)
                  val (ri, rj) = r (i,j)
                  f(redh(li, lj) , redh(ri,rj))
                }
              }
            }
            f (base, redh (0, (elems.length-1)))
          }
        }
    }
  }




//Parallel implementation of Sequence
  object ParArraySequence extends SequenceObject {

    def empty[T] : Sequence[T] = new ParArraySequenceImpl[T] (ParVector.empty)
    def singleton[T](a:T) : Sequence[T] = new ParArraySequenceImpl[T] (ParVector(a))

    def fromList[T](l:List[T]) : Sequence[T] = new ParArraySequenceImpl[T] (ParVector.tabulate
                                                         (l.length)
                                                         ((n:Int) => l(n)))

    def tabulate[T](f: Int => T) (size: Int) : Sequence[T] = new ParArraySequenceImpl(ParVector.tabulate (size) (f))

    def hidel[T] (lv: ListView[T]) = lv match {
        case NIL() => ParArraySequence.empty
        case CONS(h,t) => ParArraySequence.tabulate(i => (if (i == 0) h
                                                       else t(i-1) ))(t.length+1)
    }

    def hidet[T] (tv: TreeView[T]) = tv match {
      case EMPTY() => ArraySequence.empty
      case LEAF(x) => ArraySequence.singleton(x)
      case NODE(l,r) => ArraySequence.tabulate( i => (if (i < l.length) l(i)
                                                      else r(i-l.length)))(l.length+r.length)
    }

    private class ParArraySequenceImpl[T] (private val elems: ParVector[T]) extends Sequence[T] {

      val length = elems.length

      def apply (i: Int) = elems(i)

      def nth (i:Int) = elems(i)

      val toList = elems.toList

      def take (i:Int) = new ParArraySequenceImpl[T] (elems.take(i))

      def drop (i:Int) = new ParArraySequenceImpl[T] (elems.drop(i))

      def showl () : ListView[T]= elems.length match{
        case 0 => NIL()
        case _ => CONS (elems.head, ParArraySequence.tabulate ((i:Int) => elems(i+1))(length-1))
      }

      def showt (): TreeView[T] = elems.length match{
        case 0 => EMPTY()
        case 1 => LEAF(elems(0))
        case _ => {val n = (elems.length / 2);NODE(this.take(n), this.drop(n))}
      }

      def map[V](f: (T) => V) = new ParArraySequenceImpl[V]( elems.map(f))

      def map2[A,B] (f: ((T),A) => B) (b: Sequence[A]): Sequence[B] =
        ParArraySequence.tabulate ((i:Int) => f( elems(i), b.nth(i))) ((b.length).min(elems.length))

      def filter(p:(T) => Boolean ): Sequence[T] = new ParArraySequenceImpl[T](elems.filter(p))

      def gen_reduce(f: (T,T) => T):T = elems.reduce[T](f)

      def reduce (f: (T,T) => T)(base:T):T =
        elems.length match{
          case 0 => base
          case _ => {
            def ppt(x:Int) = {
              def ppth(curr:Int, prev:Int):Int = {
                if(curr < x) ppth(2*curr, curr)
                else prev
              }
              ppth(1,0)
            }
            def l(a:Int, b:Int) = (a, (a+ppt((b-a)+1))-1)
            def r(a:Int, b:Int) = (a+ppt((b-a)+1), b)

            def redh(i:Int, j:Int ) : T = {
              (j-i) match {
                case 0 => elems(i)
                case _ => {
                  val (li, lj) = l (i,j)
                  val (ri, rj) = r (i,j)
                  f(redh(li, lj) , redh(ri,rj))
                }
              }
            }
            f (base, redh (0, (elems.length-1)))
          }
        }

    }
  }
