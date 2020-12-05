import scala.io.Source
import scala.language.postfixOps
import java.lang.Integer

object Day5 extends App {
    def separate(ticket: String): (String, String) =
        (ticket.substring(0,7), ticket.substring(7,10))

    def convert(ticket: (String, String)): (Int, Int) = {
        def letterToDigit(letter: Char): Char = {
            letter match {
                case 'F' => '0'
                case 'B' => '1'
                case 'L' => '0'
                case 'R' => '1'
            }
        }
        ticket match {
            case (row, col) => (Integer.parseInt(row map letterToDigit, 2), Integer.parseInt(col map letterToDigit, 2))
        }
    }

    def seatID(ticket: (Int, Int)): Int = {
        ticket match {
            case (row, col) => (row * 8) + col
        }
    }

    val input: List[String] = Source.fromFile("day5in").getLines.toList
    val separated: List[(String, String)] = input map separate
    val converted: List[(Int, Int)] = separated map convert
    val seatIDs: List[Int] = converted map seatID
    println(seatIDs.max)

    val actuals = seatIDs.toSet
    val possibles = (seatIDs.min to seatIDs.max).toSet

    println(possibles &~ actuals)
}
