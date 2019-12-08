package aoc2019.day8

import aoc2019.Solution

object Part1 extends Solution[String] {
  def solution: Int = {
    val numPixelsPerLayer = 25 * 6

    val layers = line.grouped(numPixelsPerLayer)

    val layer = layers.minBy(layer => layer.count(_ == '0'))

    layer.count(_ == '1') * layer.count(_ == '2')
  }
}
