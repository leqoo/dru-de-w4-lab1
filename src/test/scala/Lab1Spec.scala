import java.nio.charset.StandardCharsets
import java.time.{LocalDate, LocalDateTime}

import org.scalatest.{FlatSpec, FunSpec, Matchers}

import scala.io.{Codec, Source}

class Lab1Spec extends FlatSpec with Matchers {

  def sourceIterator: Iterator[String] =
    Source.fromResource("UofS_access_log")(Codec.ISO8859).getLines()

  "Log Record" should "parse formatted string correctly" in {
    assert(
      LogRecord
        .parse(
          "202.32.92.47 - - [01/Jun/1995:00:00:59 -0600] \"GET /~scottp/publish.html\" 200 271")
        .nonEmpty)
    assert(sourceIterator.flatMap(LogRecord.parse).size == 2408623)
  }

  it should "not parse malformed strings" in {
    assert(LogRecord.parse("").isEmpty)
    assert(LogRecord.parse("malformed input").isEmpty)
    assert(LogRecord.parse("sdflhgsdfg").isEmpty)
    assert(LogRecord.parse("maz3.maz.net - - [11/Oa67220.dial.tip.net - - [12/Oct/1995:01:39:12 -0600] \"POST /cgi-bin/phone.pl HTTP/1.0\" 200 309").isEmpty)
    assert(LogRecord.parse("129.186.123.55 - - [12/Oct/1995ag5881.usask.ca - - [12/Oct/1995:16:07:36 -0600] \"GET /images/letter_32.gif HTTP/1.0\" 200 149").isEmpty)
  }

  val records = new Lab1(sourceIterator.flatMap(LogRecord.parse).toVector)

  "task1" should "work correctly" in {

    assert(records.task1(7000) == Set())
    assert(
      records.task1(10000) == Set(
        "GET /registrar/95_96_Calendar/Coll_of_AR/AR_promotion_Graduation.html HTTP/1.0"
      )
    )

  }

  "task2" should "work correctly" in {

    assert(records.task2("GET /~macpherc/images/lilbadge.gif") == Set(3274))
    assert(records.task2("GET /images/logo.gif HTTP/1.0") == Set(2273))

  }

  "task3" should "work correctly" in {

    assert(records.task3("GET /~macpherc/images/lilbadge.gif") == Set())
    assert(records.task3("GET /images/logo.gif HTTP/1.0") == Set())

  }

  "task4" should "work correctly" in {
    assert(records.task4("earl", LocalDate.of(1995, 6, 1)) == Set())
    assert(
      records.task4("John Thomas", LocalDate.of(1995, 12, 7)) == Set(
        "GET /dcs/courses/cai/html/introduction_lesson/index.html HTTP/1.0",
        "GET /dcs/courses/cai/html/index.html HTTP/1.0"))
  }

  "task5" should "work correctly" in {

    val result = records.task5("earl")
    val result2 = records.task5("fogel")

    assert(result == Set(LocalDate.of(1995, 8, 22)))
    assert(
      result2 == Set(LocalDate.of(1995, 8, 22),
                     LocalDate.of(1995, 9, 28),
                     LocalDate.of(1995, 10, 24),
                     LocalDate.of(1995, 11, 20),
                     LocalDate.of(1995, 12, 15)))

  }

  "task6" should "work correctly" in {

    val result = records.task6("earl", LocalDate.of(1995, 8, 1))
    val result2 = records.task6("John Thomas", LocalDate.of(1995, 12, 7))
    val result3 = records.task6("fogel", LocalDate.of(1995, 11, 14))

    assert(!result)
    assert(result2)
    assert(!result3)

  }

  "task7" should "work correctly" in {

    val dt1: LocalDate = LocalDate.of(1995, 1, 1)
    val dt2: LocalDate = dt1.plusDays(2)
    val dt3 = dt1.plusDays(1)

    val result = records.task7(dt1, dt2)
    val result2 = records.task7(dt1, dt3)
    val result3 = records.task7(dt3, dt1)

    assert(result == Vector())
    assert(result2 == Vector())
    assert(result3 == Vector())

  }

  "task8" should "work correctly" in {

    val result = records.task8
    assert(result.contains(58206298))

  }

  "task9" should "work correctly" in {

    assert(
      records.task9(2) == Vector("guest",
                                 "hbund569@csun.edu",
                                 "fogel",
                                 "lowey",
                                 "earl",
                                 "hannah",
                                 "zennon",
                                 "John Thomas",
                                 "mazzei@skyfox.usask.ca",
                                 "hbund569",
                                 "wallace"))
    assert(
      records.task9(3) == Vector("guest",
                                 "fogel",
                                 "lowey",
                                 "John Thomas",
                                 "hbund569",
                                 "wallace"))
    assert(records.task9(4) == Vector("lowey", "fogel", "wallace", "hbund569"))

  }

  "task10" should "work correctly" in {

    val dt1: LocalDate = LocalDate.of(1995, 1, 1)
    val dt2: LocalDate = LocalDate.of(1995, 1, 4)

    val result = records.task10(dt1).sorted
    val result2 = records.task10(dt2).sorted
    val result3 = records.task10(LocalDate.MIN).sorted

    assert(result == Vector())
    assert(result2 == Vector())
    assert(result3 == Vector())

  }

  "task11" should "work correctly" in {

    val result =
      records.task11(LocalDate.of(1995, 8, 11), LocalDate.of(1995, 8, 12))
    assert(
      result == Set(
        "204.112.250.133",
        "199.171.202.101",
        "140.132.92.5",
        "134.91.60.70",
        "204.248.192.22",
        "202.76.6.6",
        "204.193.132.56",
        "160.45.4.4",
        "144.92.151.241",
        "204.179.92.55",
        "202.36.37.136",
        "130.225.119.238",
        "194.128.16.2",
        "192.139.11.136",
        "198.161.85.208",
        "199.20.27.53",
        "192.139.11.101",
        "199.8.200.49",
        "128.192.8.38",
        "128.193.162.144",
        "204.174.70.21",
        "205.133.97.117",
        "204.174.70.96",
        "132.156.246.46",
        "204.174.97.83",
        "142.139.203.42",
        "149.123.74.30",
        "192.80.63.6",
        "199.212.243.40",
        "131.183.113.68",
        "138.47.53.23",
        "131.95.89.6",
        "160.205.49.126",
        "199.35.220.200",
        "193.6.129.214",
        "199.88.135.10",
        "163.121.19.36",
        "199.84.220.101",
        "199.166.232.102",
        "202.244.227.75",
        "158.144.21.4",
        "198.5.254.26",
        "142.77.70.4",
        "204.191.102.3",
        "198.109.130.132",
        "204.187.144.234",
        "205.160.81.79",
        "204.101.181.15",
        "134.245.156.166",
        "170.142.8.71",
        "149.4.2.4",
        "204.215.247.10",
        "139.57.128.111",
        "128.206.193.68",
        "128.233.136.145",
        "151.99.136.10",
        "198.14.1.98",
        "137.219.85.143",
        "204.214.225.27",
        "151.100.4.9",
        "130.69.193.53",
        "200.255.254.55",
        "204.239.199.36",
        "138.73.14.14",
        "204.185.50.41",
        "149.171.52.160",
        "137.118.10.239",
        "198.204.127.90",
        "205.230.44.59",
        "199.3.241.117",
        "204.92.3.21",
        "192.109.159.1",
        "131.92.62.95",
        "132.229.8.161",
        "131.104.64.30",
        "199.166.239.18",
        "138.73.15.212",
        "165.189.15.37",
        "134.87.21.238",
        "129.93.229.121",
        "199.128.20.35",
        "134.75.230.51",
        "192.139.11.151",
        "204.168.122.167",
        "136.205.66.96",
        "204.209.1.36",
        "132.201.8.79",
        "205.164.5.138",
        "163.200.101.45",
        "149.171.32.228",
        "204.101.86.2",
        "199.98.150.4",
        "168.221.3.10",
        "204.165.156.23",
        "204.239.245.202",
        "150.231.13.87",
        "131.217.101.126",
        "159.249.104.61",
        "152.38.252.252",
        "147.129.14.62",
        "128.187.39.179",
        "198.70.185.209",
        "205.160.191.119",
        "162.6.63.77",
        "137.82.67.4",
        "199.45.127.118",
        "128.95.121.113",
        "146.79.190.27",
        "152.97.80.106",
        "158.111.162.37",
        "147.160.2.144",
        "128.146.83.23",
        "192.101.135.111",
        "128.233.14.195",
        "142.130.2.82",
        "192.139.11.158",
        "194.47.89.10",
        "144.96.246.36",
        "192.153.34.251",
        "167.218.15.232",
        "198.247.22.1",
        "137.46.113.39",
        "192.139.11.140",
        "130.225.41.176",
        "142.139.180.179",
        "129.70.12.84",
        "16.1.0.61",
        "202.37.142.223",
        "155.64.157.68",
        "198.76.88.25",
        "198.60.253.113",
        "142.158.220.17",
        "204.198.25.111",
        "204.191.205.13",
        "192.139.11.103",
        "131.90.40.169",
        "128.233.14.201",
        "205.161.186.2",
        "204.92.246.3",
        "130.37.120.200",
        "199.249.179.23",
        "204.92.210.7",
        "128.196.157.130",
        "134.36.186.106",
        "149.8.45.203",
        "204.168.122.166",
        "168.87.23.33",
        "134.193.213.166",
        "161.142.40.78",
        "137.215.115.159",
        "131.123.20.250",
        "204.161.48.57",
        "192.139.11.198",
        "202.244.228.89",
        "194.19.36.254",
        "129.217.191.54",
        "198.247.16.1",
        "204.187.144.202",
        "203.67.31.15",
        "204.174.129.160",
        "138.86.9.23",
        "199.1.231.38",
        "198.93.92.254",
        "199.242.22.71",
        "202.88.0.29",
        "205.189.147.236",
        "192.58.194.79",
        "142.42.16.211",
        "130.207.84.59",
        "129.109.106.29",
        "142.36.204.76",
        "204.211.28.21",
        "144.80.128.6",
        "199.166.254.11",
        "128.220.62.134",
        "204.212.232.209",
        "192.219.48.22",
        "204.255.128.112",
        "131.144.28.252",
        "204.232.5.3",
        "146.176.133.136",
        "155.210.156.60",
        "134.153.11.50",
        "155.69.56.132",
        "192.152.108.74",
        "142.204.72.59",
        "142.94.24.34",
        "198.161.120.199",
        "158.152.1.40",
        "202.64.33.124",
        "205.206.80.55",
        "192.139.11.172",
        "129.215.244.156",
        "192.139.11.251",
        "144.16.79.50",
        "205.206.32.55",
        "199.60.237.16",
        "134.156.44.35",
        "204.156.141.36",
        "159.249.7.57",
        "205.218.214.53",
        "167.241.33.89",
        "204.182.134.32",
        "198.59.100.171",
        "198.53.152.113",
        "199.71.21.72",
        "192.111.222.158",
        "199.170.102.100",
        "204.185.104.4",
        "166.37.20.185",
        "128.95.144.26",
        "202.44.239.102",
        "199.60.237.134",
        "132.246.240.101",
        "198.168.189.215",
        "198.62.84.81",
        "130.209.97.41",
        "204.241.105.121",
        "144.191.173.12",
        "204.225.175.109",
        "199.78.224.19",
        "204.117.67.250",
        "164.125.60.21",
        "170.158.40.217",
        "204.239.197.164",
        "137.53.76.137",
        "199.190.76.51",
        "131.202.45.73",
        "192.54.81.3",
        "199.120.67.233",
        "137.118.10.253",
        "137.113.192.101",
        "131.247.185.16",
        "199.29.141.70",
        "139.103.80.65",
        "198.169.176.2",
        "196.3.72.63",
        "134.164.180.58",
        "130.158.145.205",
        "134.225.75.1",
        "202.96.134.236",
        "204.83.254.125",
        "143.88.179.126",
        "192.104.24.51",
        "199.2.142.58",
        "194.137.84.13",
        "206.41.131.24",
        "199.72.119.174",
        "199.190.113.5",
        "159.142.152.60",
        "138.73.15.216",
        "147.110.11.227",
        "170.161.8.16",
        "192.219.241.34",
        "141.240.30.206",
        "129.215.180.121",
        "131.251.0.1",
        "132.156.35.109",
        "138.64.123.120",
        "142.104.42.17",
        "128.23.202.182",
        "199.166.191.240",
        "128.233.136.148"
      ))

  }

  "task12" should "work correctly" in {

    val result = records.task12("GET / HTTP/1.0")
    val result2 = records.task12("GET /~macpherc/images/lilbadge.gif")
    val result3 = records.task12("?-lalala-?").sorted

    assert(
      result == Vector("duke.usask.ca",
                       "sask.usask.ca",
                       "broadway.sfn.saskatoon.sk.ca",
                       "moondog.usask.ca",
                       "herald.usask.ca"))
    assert(
      result2 == Vector("repc173.roe.ac.uk",
                        "sesame.hensa.ac.uk",
                        "geol39.usask.ca",
                        "marsh.spider.co.uk",
                        "koriel.sun.com"))
    assert(result3 == Vector())

  }

  "task13" should "work correctly" in {

    val result =
      records.task13(2, LocalDate.of(1995, 1, 3), LocalDate.of(1995, 2, 7))
    val result2 =
      records.task13(2, LocalDate.of(1995, 1, 3), LocalDate.of(1995, 3, 1))
    val result3 = records.task13(500, LocalDate.MIN, LocalDate.MAX)

    assert(result == Set())
    assert(result2 == Set())
    assert(result3 == Set("128.95.226.85", "192.139.11.254"))

  }

  "task14" should "work correctly" in {

    val result = records.task14(LocalDate.MIN, LocalDate.MAX)
    val result2 =
      records.task14(LocalDate.of(1995, 1, 19), LocalDate.of(1995, 2, 14))

    assert(
      result.contains("GET /cgi-bin/cusi?query=%20S%26M&service= HTTP/1.0"))
    assert(result2.isEmpty)

  }

  "task15" should "work correctly" in {

    val result = records.task15(LocalDate.MIN, LocalDate.MAX)
    val result2 =
      records.task15(LocalDate.of(1995, 1, 19), LocalDate.of(1995, 2, 8))

    assert(result.exists(n => (n - 0.0087037).abs < 0.00001))
    assert(result2.isEmpty)

  }
}
