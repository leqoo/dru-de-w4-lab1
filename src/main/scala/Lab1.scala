import java.time.{LocalDate, LocalDateTime}


final case class LogRecord(host: String,
                           userName: Option[String],
                           timestamp: LocalDateTime,
                           request: String,
                           replyCode: String,
                           bytesInReply: Option[Int])

object LogRecord {
  val recordRegex = """^(\S+)\s(\S+)\s(.*?)\s\[(.+)\]\s\"(.+)\"\s(\d+)\s(\d+)?(.*)$""".r 
  val dateTimeFormatter = DateTimeFormatter.ofPattern("dd/MMM/yyyy:HH:mm:ss xxxx") // 01/Jun/1995:00:25:16 -0600
  
  def parse(string: String): Option[LogRecord] = string match {
    case recordRegex(host, x, un, tss, req,rc,br,y) =>
      val unoption:Option[String] = un match {
        case "-" => None 
        case other => Option(other)
      }
      Try(LocalDateTime.parse(tss, dateTimeFormatter)).toOption.map(
        LogRecord(host, unoption, _, req, rc,Try(br.toInt).toOption ))
    case s =>
      println(s)
      None
}

class Lab1(val records: Vector[LogRecord]) {

  def task1(replyBytes: Int): Set[String] = records.withFilter(_.bytesInReply contains replyBytes).map(_.request).toSet

  def task2(request: String): Set[Int] = records.withFilter(_.request == request).flatMap(_.bytesInReply).withFilter(_>0).toSet

  def task3(request: String): Set[String] = records.withFilter(_.request == request).flatMap(_.userName).toSet

  def task4(username: String, date: LocalDate): Set[String] = 
  records.withFilter(x=>((x.userName contains username)&&(x.timestamp.toLocalDate==date))).map(_.request).toSet

  def task5(username: String): Set[LocalDate] = 
  records.withFilter(_.userName contains username).map(_.timestamp.toLocalDate).toSet 

  def task6(username: String, date: LocalDate): Boolean = 
  records.exists(x=>(x.userName contains username)&&(x.timestamp.toLocalDate==date))

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
