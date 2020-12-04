import scala.io.Source

object Day4 extends App {
    def parseLine(l: String): Map[String, String] =
        l.split(' ').map(_ split ':').collect { case Array(k, v) => (k, v) }.toMap

    def valid(line: Map[String, String]): Boolean = {
        val keys: Set[String] = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
        (keys.subsetOf(line.keySet))
    }

    val data: Array[String] = Source.fromFile("day4in").mkString.split("\\n\\n").map(_.replace("\n"," "))
    val parsed: Array[Map[String, String]] = data map parseLine
    println(parsed count valid)
}
