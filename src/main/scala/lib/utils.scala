package lib.utils

  /* Definition of an order */
  sealed abstract class Order
  case object GREATER extends Order
  case object LESS extends Order
  case object EQUAL extends Order


  object compares {
    type ord[A] = (A,A) => Order

    def IntCompare (x : Int, y: Int) : Order =
      if(x > y) GREATER else
        if(x < y) LESS else
          EQUAL
  }
