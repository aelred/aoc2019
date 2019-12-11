package aoc2019

sealed trait Direction {
  def shift(pos: Pos): Pos
  def clockwise: Direction
  def anticlockwise: Direction
}


case class Up() extends Direction {
  override def shift(pos: Pos): Pos = pos.copy(y = pos.y - 1)
  override def clockwise: Direction = Right()
  override def anticlockwise: Direction = Left()
}

case class Down() extends Direction {
  override def shift(pos: Pos): Pos = pos.copy(y = pos.y + 1)
  override def clockwise: Direction = Left()
  override def anticlockwise: Direction = Right()
}

case class Left() extends Direction {
  override def shift(pos: Pos): Pos = pos.copy(x = pos.x - 1)
  override def clockwise: Direction = Up()
  override def anticlockwise: Direction = Down()
}

case class Right() extends Direction {
  override def shift(pos: Pos): Pos = pos.copy(x = pos.x + 1)
  override def clockwise: Direction = Down()
  override def anticlockwise: Direction = Up()
}
