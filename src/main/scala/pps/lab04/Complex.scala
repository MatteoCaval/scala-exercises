package pps.lab04

trait Complex {
  def re: Double

  def im: Double

  def +(c: Complex): Complex // should implement the sum of two complex numbers..
  def *(c: Complex): Complex // should implement the product of two complex numbers

}

object Complex {
  def apply(re: Double, im: Double): Complex = new ComplexImpl(re, im) // Fill here

  def unapply(arg: Complex): Option[(Double, Double)] = Option(arg.re, arg.im)
}

object TryComplex extends App {
  val a = Array(Complex(10, 20), Complex(1, 1), Complex(7, 0))
  val c = a(0) + a(1) + a(2)
  println(c, c.re, c.im) // (ComplexImpl(18.0,21.0),18.0,21.0)
  val c2 = a(0) * a(1)
  println(c2, c2.re, c2.im) // (ComplexImpl(-10.0,30.0),-10.0,30.0)

  val prova1 = new ComplexImpl(1.0, 1.0)
  val prova2 = new ComplexImpl(12.0, 1.0)
  println(prova1.equals(prova2))

}

/** Hints:
  * - implement Complex with a ComplexImpl class, similar to PersonImpl in slides
  * - check that equality and toString do not work
  * - use a case class ComplexImpl instead, creating objects without the 'new' keyword
  * - check equality and toString now
  */
class ComplexImpl(override val re: Double,
                  override val im: Double) extends Complex {

  override def +(c: Complex): Complex = Complex(re + c.re, im + c.im)

  override def *(c: Complex): Complex = Complex(re * c.re - im * c.im, re * c.im + im * c.re)

  override def toString: String = "ComplexImpl(" + re + "," + im + ")"

  override def equals(obj: Any): Boolean = obj match {
    case that: ComplexImpl if (this.re == that.re && this.im == that.im) => true
    case _ => false
  }

}
