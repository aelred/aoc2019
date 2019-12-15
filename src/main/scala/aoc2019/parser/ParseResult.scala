package aoc2019.parser

case class ParseResult[+T](result: T, rest: String) {
  def map[S](f: T => S): ParseResult[S] = copy(f(result))
}
