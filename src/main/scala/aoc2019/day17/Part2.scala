package aoc2019.day17

import aoc2019.Solution
import aoc2019.program.Program

import scala.collection.mutable

object Part2 extends Solution[Program] {
  type Action = Either[MoveFunc, Movement]

  def solution: Long = {
    val initialMovement: Seq[Movement] = getPath(line)
    println(movementString(initialMovement))

    val initialActions = initialMovement.map(Right(_))

    val possibleMoveFunctions = (for {
      i <- 2 to initialMovement.length
      moveFunction <- initialMovement.sliding(i)
      if movementString(moveFunction).length <= 20
    } yield moveFunction).toSet

    def allReductions(actions: Seq[Action], moveFunc: MoveFunc): Seq[(Seq[Action], Seq[Movement])] = {
      val startingFunctions = actions
        .dropWhile(_.isLeft)
        .takeWhile(_.isRight)
        .map(_.toOption.get)
        .inits
        .filter(possibleMoveFunctions.contains)

      startingFunctions.map { f => (reduce(actions, moveFunc, f.map(Right(_))), f) }.toSeq
    }

    val results = for {
      (actions, a) <- allReductions(initialActions, MoveFunc.A())
      (actions, b) <- allReductions(actions, MoveFunc.B())
      (actions, c) <- allReductions(actions, MoveFunc.C())
      if actions.forall(_.isLeft)
      finalMovement = actions.map(_.swap.toOption.get)
    } yield (finalMovement, a, b, c)

    val (move, a, b, c) = results(0)

    val inputStr = s"${moveFuncString(move)}\n${movementString(a)}\n${movementString(b)}\n${movementString(c)}\nn\n"
    println("INPUT:")
    println(inputStr)
    val input = mutable.Queue.from(inputStr)

    var output: Long = 0

    line.withMemory(0 -> 2).start(input.dequeue) { next =>
      output = next()
      print(output.toChar)
    }
    println()

    output
  }

  def reduce(movements: Seq[Action], funcName: MoveFunc, func: Seq[Action]): Seq[Action] = {
    if (movements.isEmpty || func.isEmpty) {
      movements
    } else if (movements.startsWith(func)) {
      Left(funcName) +: reduce(movements.drop(func.length), funcName, func)
    } else {
      movements.head +: reduce(movements.tail, funcName, func)
    }
  }

  def getPath(program: Program): Seq[Movement] = {
    val (scaffolds, robotStart) = createScaffoldSet(program)

    var robot = robotStart
    val unvisited = mutable.Set.from(scaffolds)
    val movements = mutable.Buffer[Movement]()

    unvisited.remove(robot.pos)

    while (unvisited.nonEmpty) {
      if (scaffolds.contains(robot.nextPos)) {
        movements.append(Forward())
        robot = robot.move
        unvisited.remove(robot.pos)
      } else if (scaffolds.contains(robot.left.nextPos)) {
        movements.append(TurnLeft())
        robot = robot.left
      } else if (scaffolds.contains(robot.right.nextPos)) {
        movements.append(TurnRight())
        robot = robot.right
      }
    }

    movements.toSeq
  }

  def printActions(actions: Seq[Action]): Unit = {
    var movement = 0
    val result = mutable.Buffer[String]()

    def appendMovement(): Unit = {
      if (movement > 0) {
        result.append(movement.toString)
        movement = 0
      }
    }

    for (action <- actions) {
      action match {
        case Right(Forward()) =>
          movement += 1
        case Right(x) =>
          appendMovement()
          result.append(x.toString)
        case Left(x) =>
          appendMovement()
          result.append(x.toString)
      }
    }

    appendMovement()

    println(result.mkString(","))
  }

  def movementString(moves: Seq[Movement]): String = {
    var movement = 0
    val result = mutable.Buffer[String]()

    def appendMovement(): Unit = {
      if (movement > 0) {
        result.append(movement.toString)
        movement = 0
      }
    }

    for (move <- moves) {
      move match {
        case Forward() =>
          movement += 1
        case x =>
          appendMovement()
          result.append(x.toString)
      }
    }

    appendMovement()

    result.mkString(",")
  }

  def moveFuncString(funcs: Seq[MoveFunc]): String = {
    funcs.mkString(",")
  }
}
