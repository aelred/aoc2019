package aoc2019

import aoc2019.program.Program

import scala.collection.mutable

package object day17 {

  def createScaffoldSet(program: Program): (Set[Vec2], Robot) = {
    var pos = Vec2.zero
    var robot: Option[Robot] = None
    val scaffolds = mutable.Set[Vec2]()

    program.start() { next =>
      val char = next().toChar
      print(char)

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
