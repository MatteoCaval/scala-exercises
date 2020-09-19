package pps.lab05

import java.util.concurrent.TimeUnit

import pps.lab05.PerformanceUtils.measure

import scala.concurrent.duration.FiniteDuration

object PerformanceUtils {

  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]] {
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)
  }

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] = {
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (!msg.isEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)
  }

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)
}


object CollectionsTest extends App {


  private val SIZE = 1000000

  val range = 1 to SIZE


  /* Linear sequences: List, ListBuffer */

  measure("create List") {
    range.toList
  }
  val list = range.toList
  measure("read List size") {
    list.size
  }

  measure("create Seq") {
    (1 to SIZE).toSeq
  }
  val seq = (1 to SIZE).toSeq
  measure("read Seq size") {
    seq.size
  }


  /* Indexed sequences: Vector, Array, ArrayBuffer */
  measure("create Vector") {
    range.toVector
  }
  val vector = range.toVector
  measure("read Vector size") {
    vector.size
  }

  measure("create Array") {
    range.toArray
  }
  val array = range.toArray
  measure("read Array size") {
    array.length
  }

  /* Sets */
  measure("create Set") {
    range.toSet
  }
  val set = range.toSet
  measure("read Set size") {
    set.size
  }

  val a = range.map(x => x -> x)
  /* Maps */
  measure("create Map") {
    a.toMap
  }
  val map = a.toMap
  measure("read Map size") {
    map.size
  }


  /* Comparison */

  import PerformanceUtils._

  val lst = (1 to 1000000).toList
  val vec = (1 to 1000000).toVector
  assert(measure("lst last") {
    lst.last
  } > measure("vec last") {
    vec.last
  })

}