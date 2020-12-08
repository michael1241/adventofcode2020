import scala.io.Source
import scala.util.matching.Regex

object Day8 extends App {
    case class instruction(command: String, size: Int, traversed: Boolean)
    case class state(instrs: List[instruction], index: Int, accumulator: Int)

    val data: List[List[String]] = Source.fromFile("day8in").mkString.split("\\n").toList.map(_.split(" ").toList)
    val instructions: List[instruction] = data map {case List(a, b) => instruction(a, b.toIntOption.getOrElse(0), false)}

    def run(s: state): Either[state, Int] = {
        def nop(s: state): state = {
            state(s.instrs.updated(s.index, instruction(s.instrs(s.index).command, s.instrs(s.index).size, true)), s.index + 1, s.accumulator)
        } 
        def acc(s: state): state = {
            state(s.instrs.updated(s.index, instruction(s.instrs(s.index).command, s.instrs(s.index).size, true)), s.index + 1, s.accumulator + s.instrs(s.index).size)
        }
        def jmp(s: state): state = {
            state(s.instrs.updated(s.index, instruction(s.instrs(s.index).command, s.instrs(s.index).size, true)), s.index + s.instrs(s.index).size, s.accumulator)
        }
        if (s.instrs(s.index).traversed)
            s.accumulator
        if (s.instrs(s.index).command == "nop")
            nop(s)
        if (s.instrs(s.index).command == "acc")
            acc(s)
        if (s.instrs(s.index).command == "jmp")
            jmp(s)
    }
    val result: Either[state, Int] = run(state(instructions, 0, 0))
    Left(result)
}
