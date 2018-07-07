package args_parser

import args_parser.ArgsParser.OptionDef
import args_parser.Matches.{OptVal, Value}

import scala.collection.mutable

class Matches(matches: List[(OptionDef, OptVal)], val frees: List[String]) {
  def present(name: String): Boolean = matches.exists(_._1 == name)
  def value(name: String): Option[String] = matches.find(_._1 == name).flatMap(_._2 match {
    case Value(value) => Option(value)
    case _ => None
  })
}

private[args_parser] class Matching {
  protected[args_parser] val errors = new mutable.ListBuffer[String]()
  protected[args_parser] val matches = new mutable.ListBuffer[(OptionDef, OptVal)]()
  protected[args_parser] val frees = new mutable.ListBuffer[String]()
  def toMatches: Either[List[String], Matches] = if (errors.isEmpty) {
    Right(new Matches(matches.toList, frees.toList))
  } else {
    Left(errors.toList)
  }
}

object Matches {
  private[args_parser] trait OptVal
  private[args_parser] case class Value(value: String) extends OptVal
  private[args_parser] case class Given() extends OptVal
}