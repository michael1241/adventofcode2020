import scala.io.Source
import scala.language.postfixOps
import java.lang.Integer

object Day5 extends App {
    def convert(ticket: String): Int = {
        def letterToDigit(letter: Char): Char = {
            letter match {
                case 'F' => '0'
                case 'B' => '1'
                case 'L' => '0'
                case 'R' => '1'
            }
        }
    Integer.parseInt((ticket map letterToDigit), 2)
    }

    val input: List[String] = Source.fromFile("day5in").getLines.toList
    val converted: List[Int] = input map convert
    println(converted.max)

    val actuals = converted.toSet
    val possibles = (converted.min to converted.max).toSet

    println(possibles &~ actuals)
}
