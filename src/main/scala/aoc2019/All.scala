package aoc2019

import scala.reflect.runtime.universe

object All extends App {
  val days = 1 to 25

  val path = getClass.getPackage.getName
  val runtimeMirror = universe.runtimeMirror(getClass.getClassLoader)

  for (day <- days) {
    println(
      s"""
         |
         |
         |========
         | DAY $day
         |========""".stripMargin)

    val dayPath = s"$path.day$day"

    println(
      s"""
         |
         | Part 1
         | ------""".stripMargin)

    val part1 = s"$dayPath.Part1"
    try {
      val module1 = runtimeMirror.staticModule(part1)
      val obj1 = runtimeMirror.reflectModule(module1)
      obj1.instance.asInstanceOf[Solution[_]].run(logging = false)
    } catch {
      case _: ScalaReflectionException => println("NOT FOUND")
    }

    println(
      s"""
         |
         | Part 2
         | ------""".stripMargin)

    val part2 = s"$dayPath.Part2"
    try {
    val module2 = runtimeMirror.staticModule(part2)
    val obj2 = runtimeMirror.reflectModule(module2)
    obj2.instance.asInstanceOf[Solution[_]].run(logging = false)
    } catch {
      case _: ScalaReflectionException => println("NOT FOUND")
    }
  }
}
