package aoc2019.parser

import aoc2019.parser.Parser.{RepeatParser, nothing}

import scala.Option.when
import scala.collection.mutable
import scala.util.matching.Regex

trait Parser[+T] {
  def apply(input: String): Either[Exception, ParseResult[T]]

  def ~[S](parser: => Parser[S]): Parser[(T, S)] = in => {
    for {
      r1 <- this(in)
      r2 <- parser(r1.rest)
    } yield ParseResult((r1.result, r2.result), r2.rest)
  }

  def <~[S](parser: => Parser[S]): Parser[T] = this ~ parser >> (_._1)

  def ~>[S](parser: => Parser[S]): Parser[S] = this ~ parser >> (_._2)

  def |[S >: T](parser: => Parser[S]): Parser[S] = in => {
    this(in) match {
      case Left(err1) => parser(in).left map { err2 => new Exception(s"$err1 OR $err2", err1) }
      case x => x
    }
  }

  def >>[S](f: T => S): Parser[S] = map(f)

  def map[S](f: T => S): Parser[S] = in => this(in).map(_.map(f))

  def option: Parser[Option[T]] = this.map(Some(_)) | nothing.map(_ => None)

  def repeat: Parser[List[T]] = new RepeatParser(this)

  def separatedBy(splitter: => Parser[_]): Parser[List[T]] = {
    val repeat = (this <~ splitter).repeat
    val optionalEnd = this <~ splitter.option

    repeat ~ optionalEnd >> {case x ~ y => x appended y}
  }

  def lines: Parser[List[T]] = separatedBy("\n")
}

object ~ {
  def unapply[T, S](tuple: (T, S)): Option[(T, S)] = Some(tuple)
}

object Parser {

  def parse[T: Parser](input: String): Either[Exception, T] = {
    (implicitly[Parser[T]] <~ eof)(input).map(_.result)
  }

  val word: Parser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r

  val digit: Parser[Int] = "[0-9]".r >> { _.toInt }

  val number: Parser[String] = "[+-]?[0-9]+".r

  def lit(string: String): Parser[Unit] = Literal(string)

  def regex(regex: Regex): Parser[String] = RegexParser(regex)

  implicit val nothing: Parser[Unit] = in => Right(ParseResult((), in))

  implicit val string: Parser[String] = in => Right(ParseResult(in, ""))

  implicit val int: Parser[Int] = number >> { _.toInt }

  implicit val long: Parser[Long] = number >> { _.toLong }

  implicit val range: Parser[Range] = int ~ "-" ~ int >> { case left ~ _ ~ right => left to right }

  implicit def lineParser[T](implicit parser: Parser[T]): Parser[List[T]] = parser.lines

  implicit val eof: Parser[Unit] = in => {
    when(in.isEmpty) {
      ParseResult((), in)
    } toRight new Exception(s"Expected end of file, but was $in")
  }

  implicit class Literal(string: String) extends Parser[Unit] {
    override def apply(input: String): Either[Exception, ParseResult[Unit]] = {
      when(input.startsWith(string)) {
        ParseResult((), input.substring(string.length))
      } toRight new Exception(s"Expected literal '$string', but was '$input'")
    }
  }

  implicit class RegexParser(regex: Regex) extends Parser[String] {
    override def apply(input: String): Either[Exception, ParseResult[String]] = {
      regex.findPrefixMatchOf(input) map { m =>
        ParseResult(m.matched, m.after.toString)
      } toRight new Exception(s"Expected to match '$regex', but was '$input'")
    }
  }

  private class RepeatParser[T](parser: Parser[T]) extends Parser[List[T]] {
    override def apply(input: String): Either[Exception, ParseResult[List[T]]] = {
      var rest = input
      val results = mutable.Buffer[T]()

      while (true) {
        parser(rest) match {
          case Right(ParseResult(result, r)) =>
            results.append(result)
            rest = r
          case _ =>
            return Right(ParseResult(results.toList, rest))
        }
      }

      throw new Exception("Unreachable")
    }
  }
}