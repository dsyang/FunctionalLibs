import org.scalacheck._
import lib.utils._


object ComparesSpecifications extends Properties("Compares") {
  import Prop._

   property("int less ") = forAll { (n: Int, m: Int) =>
     (n < m) ==> (compares.IntCompare(n,m) == LESS)
   }


   property("int greater ") = forAll { (n: Int, m: Int) =>
     (n > m) ==> (compares.IntCompare(n,m) == GREATER)
   }


  property("int equal ton") = forAll { (n: Int) =>
    (compares.IntCompare(n,n) == EQUAL)
  }


}

