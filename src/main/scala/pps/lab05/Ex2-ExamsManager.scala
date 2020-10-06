package pps.lab05

object ExamsManagerTest extends App {

  /* See: https://bitbucket.org/mviroli/oop2018-esami/src/master/a01b/e1/Test.java */

  sealed trait ExamResult {
    def evaluation: Option[Int]

    def cumLaude: Boolean
  }

  object ExamResult {
    def failed() = Failed

    def retired() = Retired

    def succeededCumLaude() = Succeeded(Some(30), cumLaude = true)

    def succeeded(evaluation: Int) = Succeeded(Some(evaluation), cumLaude = false)
  }

  case object Retired extends ExamResult {
    override val evaluation: Option[Int] = None
    override val cumLaude: Boolean = false
  }

  case object Failed extends ExamResult {
    override val evaluation: Option[Int] = None
    override val cumLaude: Boolean = false
  }

  case class Succeeded(evaluation: Option[Int], cumLaude: Boolean) extends ExamResult


  trait ExamsManager {
    def createNewCall(call: String): Unit

    def addStudentResult(call: String, student: String, result: ExamResult): Unit

    def getAllStudentsFromCall(call: String): Option[Seq[String]]

    def getEvaluationsMapFromCall(call: String): Option[Map[String, Int]]

    def getResultsMapFromStudent(student: String): Map[String, ExamResult]

    def getBestResultFromStudent(student: String): Option[Integer]
  }

  class ExamManagerImpl extends ExamsManager {

    private var map: Map[String, Seq[(String, ExamResult)]] = Map()

    override def createNewCall(call: String): Unit = map = map + (call -> Seq.empty)

    override def addStudentResult(call: String, student: String, result: ExamResult): Unit = {
      map = map.map(
        x => if (x._1 == call) {
          x._1 -> x._2.+:(student, result)
        } else x

      )
    }

    override def getAllStudentsFromCall(call: String): Option[Seq[String]] = map.get(call).map(_.map(_._1))

    override def getEvaluationsMapFromCall(call: String): Option[Map[String, Int]] = ??? // map.get(call).map(_.toMap)

    override def getResultsMapFromStudent(student: String): Map[String, ExamResult] = ???/*map
      .mapValues(_.find(_._1 == student))
      .filter(_._2.isDefined)
      .map(x => (x._1, x._2.get._2))*/


    override def getBestResultFromStudent(student: String): Option[Integer] = ???
  }

}
