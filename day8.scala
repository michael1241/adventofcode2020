import scala.io.Source
import scala.util.matching.Regex

object Day8 extends App {
    case class Instruction(command: String, size: Int, traversed: Boolean)
    case class State(instrs: List[Instruction], index: Int, accumulator: Int)

    val data: List[List[String]] = Source.fromFile("day8in").mkString.split("\\n").toList.map(_.split(" ").toList)
    val instructions: List[Instruction] = data collect {case List(a, b) => Instruction(a, b.toIntOption.getOrElse(0), false)}

    def run(s: State): Int = {
        def nop(s: State): State = {
            State(s.instrs.updated(s.index, Instruction(s.instrs(s.index).command, s.instrs(s.index).size, true)), s.index + 1, s.accumulator)
        } 
        def acc(s: State): State = {
            State(s.instrs.updated(s.index, Instruction(s.instrs(s.index).command, s.instrs(s.index).size, true)), s.index + 1, s.accumulator + s.instrs(s.index).size)
        }
        def jmp(s: State): State = {
            State(s.instrs.updated(s.index, Instruction(s.instrs(s.index).command, s.instrs(s.index).size, true)), s.index + s.instrs(s.index).size, s.accumulator)
        }
        if (s.instrs(s.index).traversed){
            s.accumulator
        }
        else if (s.instrs(s.index).command == "nop"){
            run(nop(s))
        }
        else if (s.instrs(s.index).command == "acc"){
            run(acc(s))
        }
        else if (s.instrs(s.index).command == "jmp"){
            run(jmp(s))
        }
        else -1
    }
    val result: Int = run(State(instructions, 0, 0))
    println(result)
}
