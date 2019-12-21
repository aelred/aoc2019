package aoc2019.day8

import aoc2019.Solution

object Part2 extends Solution[String] {
  def solution: String = {
    val numPixelsPerLayer = 25 * 6

    val layers = input.grouped(numPixelsPerLayer).toSeq

    val finalLayer = new StringBuilder(layers(0))

    for (layer <- layers) {
      for (index <- 0 until numPixelsPerLayer) {
        if (finalLayer(index) == '2') {
          finalLayer(index) = layer(index)
        }
      }
    }

    finalLayer.toString
      .replaceAll("0", " ")
      .replaceAll("1", "█")
      .grouped(25)
      .mkString("\n")
  }
}
