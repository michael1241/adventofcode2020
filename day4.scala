import scala.io.Source
import scala.util.matching.Regex

object Day4 extends App {
    case class Person(byr: Int, iyr: Int, eyr: Int, hgt: String, hcl: String, ecl: String, pid: Int)

    def parseLine(l: String): Map[String, String] =
        l.split(' ').map(_ split ':').collect { case Array(k, v) => (k, v) }.toMap

    def makePerson(l: Map[String, String]): Option[Person] = {
        for {
            byr <- l("byr").toIntOption
            iyr <- l("iyr").toIntOption
            eyr <- l("eyr").toIntOption
            hgt <- l("hgt")
            hcl <- l("hcl")
            ecl <- l("ecl")
            pid <- l("pid").toIntOption
        } yield Some(Person(byr, iyr, eyr, hgt, hcl, ecl, pid))
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
