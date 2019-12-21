package aoc2019

import scala.reflect.runtime.universe
import scala.util.Try

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

    val solution = getSolutionAtPath(dayPath) orElse getSolutionAtPath(s"$dayPath.Solution")

    solution match {
      case Some(s) => s.run(logging=false)
      case None    => println("NOT FOUND")
    }
  }

  def getSolutionAtPath(path: String): Option[Solution[_]] = Try {
      val moduleSymbol = runtimeMirror.staticModule(path)
      val module = runtimeMirror.reflectModule(moduleSymbol)
      module.instance.asInstanceOf[Solution[_]]
  }.toOption
}
