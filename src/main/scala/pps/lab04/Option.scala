package pps.lab04

object Optionals extends App {

  sealed trait Option[A] // An Optional data type
  object Option {

    case class None[A]() extends Option[A]

    case class Some[A](a: A) extends Option[A]

    def isEmpty[A](opt: Option[A]): Boolean = opt match {
      case None() => true
      case _ => false
    }

    def getOrElse[A, B >: A](opt: Option[A], orElse: B): B = opt match {
      case Some(a) => a
      case _ => orElse
    }

    def flatMap[A, B](opt: Option[A])(f: A => Option[B]): Option[B] = opt match {
      case Some(a) => f(a)
      case _ => None()
    }

    def filter[A](o: Option[A])(predicate: A => Boolean): Option[A] = o match {
      case Some(a) if (predicate(a)) => Some(a)
      case _ => None()
    }

    def filter2[A](o: Option[A])(predicate: A => Boolean): Option[A] = flatMap(o)(i => if (predicate(i)) Some(i) else None())


    def map[A, B](o: Option[A])(f: A => B): Option[B] = o match {
      case Some(a) => Some(f(a))
      case _ => None()
    }

    //    def map[A, B](o: Option[A])(predicate: A => B) = flatMap(o)(a => Some(predicate(a)))

    def map2[A, B, C](o1: Option[A], o2: Option[B])(f: (A, B) => C) = (o1, o2) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None()

    }
  }

  import Option._

  val s1: Option[Int] = Some(1)
  val s2: Option[Int] = Some(2)
  val s3: Option[Int] = None()

  println(s1) // Some(1)
  println(getOrElse(s1, 0), getOrElse(s3, 0)) // 1,0
  println(flatMap(s1)(i => Some(i + 1))) // Some(2)
  println(flatMap(s1)(i => flatMap(s2)(j => Some(i + j)))) // Some(3)
  println(flatMap(s1)(i => flatMap(s3)(j => Some(i + j)))) // None

  println(filter2(Some(5))(_ > 2))
  println(filter2(Some(5))(_ > 8))

  println(map(Some(5))(_ > 2)) // Some(true)
  println(map(None[Int])(_ > 2)) // None

  println(map2(Some(5), Some("Ciao"))((i: Int, j: String) => i + j))
}
