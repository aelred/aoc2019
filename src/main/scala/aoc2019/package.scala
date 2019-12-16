import scala.collection.{MapOps, MapView, mutable}

package object aoc2019 {
  def mapToString(map: MapView[Vec2, Char]): String = {
    val xs = map.keys.map(_.x)
    val ys = map.keys.map(_.y)
    val left = xs.minOption.getOrElse(0)
    val top = ys.minOption.getOrElse(0)
    val width = xs.maxOption.getOrElse(0) - left + 1
    val height = ys.maxOption.getOrElse(0) - top + 1

    val table = mutable.Seq.fill(height, width)(' ')

    for ((pos, char) <- map) {
      table(pos.y - top)(pos.x - left) = char
    }

    table.map(_.mkString).mkString("\n")
  }
}
