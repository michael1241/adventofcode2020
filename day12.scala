import scala.io.Source

object Day12 extends App {
    case class Instruction(command: String, size: Int, dir: String)

    def parse(line: String): Option[Instruction] = {
        line.splitAt(1) match {
            case (command, size) => Some(Instruction(command, size.toIntOption.getOrElse(0), "E"))
            case _ => None
        }
    }

    def rotate(list: List[String], by: Int) = {
    //stolen code https://stackoverflow.com/questions/31091536/built-in-method-for-rolling-scala-list-or-seq
        val shift = (by % list.size + list.size) % list.size
        val (l, r) = list.splitAt(shift)
        r ++ l
    }

    def move(instr: Instruction): (Int, Int) = {
        instr.command match {
            case "N" => (0, instr.size)
            case "E" => (instr.size, 0)
            case "S" => (0, -instr.size)
            case "W" => (-instr.size, 0)
            case "F" => move(instr.copy(command = instr.dir))
            case _ => (0, 0)
        }
    }

    val compass: List[String] = List("N", "E", "S", "W")
    val compassStart: Map[String, Int] = Map("N" -> 0, "E" -> 1, "S" -> 2, "W" -> 3)

    def dirConvert(instr: Instruction, dir: String): Instruction = {
        instr.command match {
            case "R" => instr.copy(dir=rotate(compass, (compassStart(dir) + (instr.size / 90)))(0))
            case "L" => instr.copy(dir=rotate(compass, (compassStart(dir) - (instr.size / 90)))(0))
            case _ => instr.copy(dir=dir)
        }
    }

    def setDirs(data: List[Instruction], dir: String, output: List[Instruction]): List[Instruction] = {
        data match {
            case head :: Nil =>
                val updated: Instruction = dirConvert(head, dir)
                output :+ updated
            case head :: rest =>
                val updated: Instruction = dirConvert(head, dir)
                setDirs(rest, updated.dir, output :+ updated)
            case _ => Nil
        }
    }

    val data: List[Instruction] = Source.fromFile("day12in").mkString.split("\\n").toList.flatMap(parse)
    val directioned: List[Instruction] = setDirs(data, data(0).dir, List[Instruction]())
    println(directioned.map(move(_)).foldLeft(0, 0)((a, b) => (a._1 + b._1, a._2 + b._2)))
}
