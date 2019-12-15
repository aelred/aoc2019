package aoc2019.parser

import aoc2019.parser.Parser.nothing
import aoc2019.program.Program

import scala.Option.when
import scala.util.matching.Regex

trait Parser[+T] {
  def apply(input: String): Either[String, ParseResult[T]] = parse(input).toRight(input)

  def parse(input: String): Option[ParseResult[T]]

  def ~[S](parser: => Parser[S]): Parser[(T, S)] = in => {
    for {
      r1 <- parse(in)
      r2 <- parser.parse(r1.rest)
    } yield ParseResult((r1.result, r2.result), r2.rest)
  }

  def <~[S](parser: => Parser[S]): Parser[T] = this ~ parser |-> (_._1)

  def ~>[S](parser: => Parser[S]): Parser[S] = this ~ parser |-> (_._2)

  def |[S >: T](parser: Parser[S]): Parser[S] = in => {
    this.parse(in) orElse parser.parse(in)
  }

  def |->[S](f: T => S): Parser[S] = map(f)

  def map[S](f: T => S): Parser[S] = in => parse(in).map(_.map(f))

  def option: Parser[Option[T]] = this.map(Some(_)) | nothing.map(_ => None)

  def repeat: Parser[Seq[T]] = this ~ repeat map {case x ~ y => x +: y}

  def split(splitter: Parser[_]): Parser[Seq[T]] = {
    val repeat = (this ~ splitter).map(_._1).repeat
    val optionalEnd = (this ~ splitter.option).map(_._1)

    repeat ~ optionalEnd map {case x ~ y => x :+ y}
  }
}

object ~ {
  def unapply[T, S](tuple: (T, S)): Option[(T, S)] = Some(tuple)
}

object Parser {

  def parse[T: Parser](input: String): Option[T] = {
    implicitly[Parser[T]].apply(input).toOption.map(_.result)
  }

  def lit(string: String): Parser[Unit] = Literal(string)

  def regex(regex: Regex): Parser[String] = RegexParser(regex)

  implicit val nothing: Parser[Unit] = in => Some(ParseResult((), in))

  implicit val string: Parser[String] = in => Some(ParseResult(in, ""))

  implicit val int: Parser[Int] = "[+-]?[0-9]+".r map { _.toInt }

  implicit val long: Parser[Long] = "[+-]?[0-9]+".r map { _.toLong }

  implicit val range: Parser[Range] = int ~ "-" ~ int map { case left ~ _ ~ right => left to right }

  implicit val program: Parser[Program] = long.split(",") map { new Program(_) }

  implicit val eof: Parser[Unit] = in => {
    when(in.isEmpty) {
      ParseResult((), in)
    }
  }

  implicit class Literal(string: String) extends Parser[Unit] {
    override def parse(input: String): Option[ParseResult[Unit]] = {
      when(input.startsWith(string)) {
        ParseResult((), input.substring(string.length))
      }
    }
  }

  implicit class RegexParser(regex: Regex) extends Parser[String] {
    override def parse(input: String): Option[ParseResult[String]] = {
      regex.findPrefixMatchOf(input).map(m => ParseResult(m.matched, m.after.toString))
    }
  }
}