import java.time.{LocalDate, LocalDateTime}


final case class LogRecord(host: String,
                           userName: Option[String],
                           timestamp: LocalDateTime,
                           request: String,
                           replyCode: String,
                           bytesInReply: Option[Int])

object LogRecord {
  def parse(string: String): Option[LogRecord] = None
}

class Lab1(val records: Vector[LogRecord]) {

  def task1(replyBytes: Int): Set[String] = Set()

  def task2(request: String): Set[Int] = Set()

  def task3(request: String): Set[String] = Set()

  def task4(username: String, date: LocalDate): Set[String] = Set()

  def task5(username: String): Set[LocalDate] = Set()

  def task6(username: String, date: LocalDate): Boolean = false

  def task7(startDate: LocalDate, endDate: LocalDate): Vector[String] = Vector()

  def task8: Option[Int] = None

  def task9(n: Int): Vector[String] = Vector()

  def task10(date: LocalDate): Vector[String] = Vector()

  def task11(startDate: LocalDate, endDate: LocalDate): Set[String] = Set()

  def task12(request: String): Vector[String] = Vector()

  def task13(n: Int, startDate: LocalDate, endDate: LocalDate): Set[String] = Set()

  def task14(startDate: LocalDate, endDate: LocalDate): Option[String] = None

  def task15(startDate: LocalDate, endDate: LocalDate): Option[Double] = None
}
