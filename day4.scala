import scala.io.Source
import scala.util.matching.Regex

object Day4 extends App {
    case class Person(byr: Int, iyr: Int, eyr: Int, hgt: String, hcl: String, ecl: String, pid: Int)

    def parseLine(l: String): Map[String, String] =
        l.split(' ').map(_ split ':').collect { case Array(k, v) => (k, v) }.toMap

    def makePerson(l: Map[String, String]): Option[Person] = {
        for {
            byr <- l.get("byr").flatMap(_.toIntOption)
            iyr <- l.get("iyr").flatMap(_.toIntOption)
            eyr <- l.get("eyr").flatMap(_.toIntOption)
            hgt <- l.get("hgt")
            hcl <- l.get("hcl")
            ecl <- l.get("ecl")
            pid <- l.get("pid").flatMap(_.toIntOption)
        } yield Person(byr, iyr, eyr, hgt, hcl, ecl, pid)
    }

    def validateFields(l: Map[String, String]): Option[Person] = {
        val keys: Set[String] = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
        keys.subsetOf(l.keySet) match {
            case true => makePerson(l)
            case _ => None
        }
    }

    val data: Array[String] = Source.fromFile("day4in").mkString.split("\\n\\n").map(_.replace("\n"," "))
    val parsed: Array[Map[String, String]] = data map parseLine
    val validated: Array[Person] = parsed flatMap validateFields
    println(validated.length)
}
