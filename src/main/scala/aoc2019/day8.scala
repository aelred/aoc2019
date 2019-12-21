package aoc2019

object day8 extends Solution[String] {

  def part1: Int = {
    val numPixelsPerLayer = 25 * 6

    val layers = input.grouped(numPixelsPerLayer)

    val layer = layers.minBy(layer => layer.count(_ == '0'))

    layer.count(_ == '1') * layer.count(_ == '2')
  }

  def part2: String = {
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
