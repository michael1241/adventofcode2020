import scala.io.Source
import scala.util.matching.Regex

object Day4 extends App {
    def validate(result: Option[List[String]]): Boolean = {
        true
    }
    val data: Array[String] = Source.fromFile("day4in").mkString.split("\\n\\n").map(_.replace("\n"," "))
    val reg: Regex = "(?=.*byr:)(?=.*iyr:)(?=.*eyr:)(?=.*hgt:)(?=.*hcl:)(?=.*ecl:)(?=.*pid:)".r.unanchored
    val reg2: Regex = "(?=.*byr:([^ ]*))(?=.*iyr:([^ ]*))(?=.*eyr:([^ ]*))(?=.*hgt:([^ ]*))(?=.*hcl:([^ ]*))(?=.*ecl:([^ ]*))(?=.*pid:([^ ]*))".r.unanchored
    println( data count {
        case reg() => true
        case _ => false}
    )
}
