import scala.io.Source
import scala.util.matching.Regex

object Day4 extends App {
    def parseLine(l: String): Map[String, String] =
        l.split(' ').map(_ split ':').collect { case Array(k, v) => (k, v) }.toMap

    def valid(line: Map[String, String]): Boolean = {
        val keys: Set[String] = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

        if (!keys.subsetOf(line.keySet)) return false

        val hgt: Int = line("hgt").filter(_.isDigit).toInt
        val hgttype: String = line("hgt").filter(_.isLetter).toString
        (
            line("byr").toInt >= 1920 && line("byr").toInt <= 2002 &&
            line("iyr").toInt >= 2010 && line("iyr").toInt <= 2020 &&
            line("eyr").toInt >= 2020 && line("eyr").toInt <= 2030 &&
            ((hgttype == "cm" && hgt >= 150 && hgt <= 193) || (hgttype == "in" && hgt >= 59 && hgt <= 76)) &&
            line("hcl").matches("#[0-9a-f]{6}") &&
            line("ecl").matches("amb|blu|brn|gry|grn|hzl|oth") &&
            line("pid").matches("[0-9]{9}")
        )
    }

    val data: Array[String] = Source.fromFile("day4in").mkString.split("\\n\\n").map(_.replace("\n"," "))
    val parsed: Array[Map[String, String]] = data map parseLine
    println(parsed count valid)
}
