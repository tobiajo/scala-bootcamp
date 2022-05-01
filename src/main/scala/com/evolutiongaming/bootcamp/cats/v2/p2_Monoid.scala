package com.evolutiongaming.bootcamp.cats.v2

object p2_Monoid {

  /**
    * Monoid is a Semigroup but with an `empty` element defined.
   **/
  import cats.Monoid

  /**
    * Ex 2.0 implement a monoid with `*` (multiplication) as an operation.
    * Q: Can division be picked as an associative binary operation?
   **/
  val intMonoid: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 1

    override def combine(x: Int, y: Int): Int = x * y
  }

  /**
    * Ex 2.1 use string concatenation as an operation
   **/
  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def empty: String = ""

    override def combine(x: String, y: String): String = x + y
  }

  /**
    * Ex 2.2 How about a monoid for boolean?
    * Pick AND as a binary operation.
    *
    * Q: How many monoids exist for boolean?
   **/

  val boolMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  // as you might already guessed, there are plenty of instances already defined in cats library:

  import cats.instances.int._
  import cats.instances.option._

  val intOptMonoid: Monoid[Option[Int]] = implicitly[Monoid[Option[Int]]]
}
