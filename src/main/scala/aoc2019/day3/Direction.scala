package aoc2019.day3

sealed trait Direction {
  def shift(pos: (Int, Int)): (Int, Int)
}


case class Up() extends Direction {
  override def shift(pos: (Int, Int)): (Int, Int) = (pos._1, pos._2 - 1)
}

case class Down() extends Direction {
  override def shift(pos: (Int, Int)): (Int, Int) = (pos._1, pos._2 + 1)
}

case class Left() extends Direction {
  override def shift(pos: (Int, Int)): (Int, Int) = (pos._1 - 1, pos._2)
}

case class Right() extends Direction {
  override def shift(pos: (Int, Int)): (Int, Int) = (pos._1 + 1, pos._2)
}
