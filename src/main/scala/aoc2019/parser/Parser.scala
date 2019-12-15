package aoc2019.parser

import aoc2019.parser.Parser.nothing
import aoc2019.program.Program

import scala.util.matching.Regex

trait Parser[+T] {
  def apply(input: String): Either[String, ParseResult[T]] = parse(input)

  def parse(input: String): Either[String, ParseResult[T]]

  def +[S](parser: => Parser[S]): Parser[(T, S)] = in => {
    for {
      r1 <- this.parse(in)
      r2 <- parser.parse(r1.rest)
    } yield ParseResult((r1.result, r2.result), r2.rest)
  }

  def |[S >: T](parser: Parser[S]): Parser[S] = in => {
    this.parse(in) orElse parser.parse(in)
  }

  def map[S](f: T => S): Parser[S] = in => parse(in).map(_.map(f))

  def option: Parser[Option[T]] = this.map(Some(_)) | nothing.map(_ => None)

  def repeat: Parser[Seq[T]] = (this + repeat) map {case x + y => x +: y}

  def split(splitter: Parser[_]): Parser[Seq[T]] = {
    val repeat = (this + splitter).map(_._1).repeat
    val optionalEnd = (this + splitter.option).map(_._1)

    (repeat + optionalEnd) map {case x + y => x :+ y}
  }
}

object + {
  def unapply[T, S](tuple: (T, S)): Option[(T, S)] = Some(tuple)
}

object Parser {

  def parse[T: Parser](input: String): Option[T] = {
    implicitly[Parser[T]].parse(input).toOption.map(_.result)
  }

  def lit(string: String): Parser[Unit] = in => {
    if (in.startsWith(string)) {
      Right(ParseResult((), in.substring(string.length)))
    } else {
      Left(in)
    }
  }

  def regex(regex: Regex): Parser[String] = in => {
    regex.findPrefixMatchOf(in).map(m => ParseResult(m.matched, m.after.toString)).toRight(in)
  }

  implicit val nothing: Parser[Unit] = in => Right(ParseResult((), in))

  implicit val string: Parser[String] = in => Right(ParseResult(in, ""))

  implicit val int: Parser[Int] = regex("[+-]?[0-9]+".r).map(_.toInt)

  implicit val long: Parser[Long] = regex("[+-]?[0-9]+".r).map(_.toLong)

  implicit val range: Parser[Range] = (int + lit("-") + int) map { case left + _ + right => left to right }

  implicit val program: Parser[Program] = long.split(lit(",")).map(new Program(_))

  implicit val eof: Parser[Unit] = in => {
    if (in.isEmpty) {
      Right(ParseResult((), in))
    } else {
      Left(in)
    }
  }
}