import scala.io.Source

object Day14 extends App {
    case class Instruction(address: Int, value: Int)

    def translate(char: Char): Option[Int] = {
        char match {
            case '1' => Some(1)
            case '0' => Some(0)
            case 'X' => None
            }
    }

    def toMask(maskstring: String): List[Option[Int]] = {
        {maskstring map translate}.toList
    }

    def parse(input: String): Either[List[Option[Int]], Instruction] = {
        input match {
            case s"mask = ${mask}" => Left(toMask(mask))
            case s"mem[${address}] = ${value}" => Right(Instruction(address.toIntOption.getOrElse(0), value.toIntOption.getOrElse(0)))
        }
    }

    val data: List[Either[List[Option[Int]], Instruction]] = Source.fromFile("day14in").mkString.split("\\n").toList.map(parse(_))

    def compare(valbit: Option[Int], maskbit: Option[Int]): Int = {
        maskbit match {
            case None => valbit.get
            case _ => maskbit.get
        }
    }

    def applyMask(value: Int, mask: List[Option[Int]]): Int = {
        val binvalue: List[Option[Int]] = {value.toBinaryString map translate}.toList
        for {
            (valbit, maskbit) <- binvalue.reverse.zipAll(mask.reverse, Some(0), Some(0))
        }
        //println(valbit, maskbit)
        println(compare(valbit, maskbit))
        1
    }
    applyMask(17, List(None, None, None, None, None, None))

    def addElement(memory: Map[Int,Int], instr: Instruction, mask: List[Option[Int]]): Map[Int, Int] = {
        val newMemory: Map[Int, Int] = Map(instr.address -> applyMask(instr.value, mask))
        (memory ++ newMemory)
    }

    def process(data: List[Either[List[Option[Int]], Instruction]], mask: List[Option[Int]], memory: Map[Int, Int]): Map[Int, Int] = {
        data match {
            case Nil => memory
            case Left(head) :: tail => process(tail, head, memory)
            case Right(head) :: tail =>
                val updatedmemory: Map[Int,Int] = addElement(memory, head, mask)
                process(tail, mask, updatedmemory)
        }
    }
}
