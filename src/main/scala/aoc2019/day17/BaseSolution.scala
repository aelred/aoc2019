package aoc2019.day17

import aoc2019.{Down, Solution, Up, Left, Right, Vec2}
import aoc2019.program.Program

import scala.collection.mutable

trait BaseSolution extends Solution[Program] {

  def createScaffoldSet(program: Program): (Set[Vec2], Robot) = {
    var pos = Vec2.zero
    var robot: Option[Robot] = None
    val scaffolds = mutable.Set[Vec2]()

    program.run() { next =>
      val char = next().toChar
      logRaw(char)

      if (char == '\n') {
        pos = pos.down.copy(x = 0)
      } else {

        val robotDir = char match {
          case 'X' => throw new Exception("Robot has already tumbled into space!")
          case '^' => Some(Up())
          case 'v' => Some(Down())
          case '>' => Some(Right())
          case '<' => Some(Left())
          case _ => None
        }

        robot = robot orElse (robotDir map { Robot(pos, _) })

        char match {
          case '.' | 'X' => ()
          case '#' | '^' | 'v' | '<' | '>' => scaffolds.add(pos)
        }

        pos = pos.right
      }
    }

    (scaffolds.toSet, robot.get)
  }
}
