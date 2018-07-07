package args_parser

import args_parser.ArgsParser.OptionDef
import args_parser.Matches.{Given, Value}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.parsing.combinator._

class ArgsParser {
  private val optDefinitions = new mutable.ListBuffer[OptionDef]()

  def add(shortName: String, name: String, usage: String): ArgsParser = {
    def toOption(str: String): Option[String] = {
      if (str == null || str == "") {
        None
      } else {
        Option(str)
      }
    }

    val valueOption = ParserImpl.parse(ParserImpl.valueOption, s"--$name")
    val optDef = if (valueOption.successful) {
      val (opt, value) = valueOption.get
      OptionDef(toOption(shortName), toOption(opt), usage, Option(value))
    } else {
      OptionDef(toOption(shortName), toOption(name), usage, None)
    }
    optDefinitions.append(optDef)
    this
  }

  def showUsage(): Unit = {
    val padLength = optDefinitions.foldLeft(0)((a, b) => {
      val length = b.usageName.length
      if (length > a) {
        length
      } else {
        a
      }
    })
    optDefinitions.foreach(_.showUsage(padLength))
  }

  def parse(args: Array[String]): Either[List[String], Matches] = {
    val result = new Matching()
    if (args.length > 0) {
      parseImpl(args.head, args.tail, result).toMatches
    }
    result.toMatches
  }

  @tailrec
  private def parseImpl(head: String, tail: Array[String], matches: Matching): Matching = {
    val matched = matchImpl(head, matches)
    if (matched.isEmpty) {
      matches.frees.append(head)
    }
    if (tail.length == 0) {
      matches
    } else {
      parseImpl(tail.head, tail.tail, matches)
    }
  }

  private def matchImpl(str: String, matches: Matching): Option[String] = {
    val shortOptions = ParserImpl.parse(ParserImpl.shortOptions, str)
    if (shortOptions.successful) {
      val shortOption = shortOptions.get
      val optDefinitions = shortOption.toCharArray.map(c => (c, getOptionDef(c.toString)))
      optDefinitions.foreach(e  => e._2 match {
        case Some(optionDef) => matches.matches.append((optionDef, Given()))
        case None => matches.errors.append(e._1.toString)
      })
      return Option(shortOption)
    }

    val valueOption = ParserImpl.parse(ParserImpl.valueOption, str)
    if (valueOption.successful) {
      val (opt, value) = valueOption.get
      getOptionDef(opt) match {
        case Some(optionDef) => matches.matches.append((optionDef, Value(value)))
        case None => matches.errors.append(opt)
      }
      return Option(opt)
    }

    val longOption = ParserImpl.parse(ParserImpl.longOption, str)
    if (longOption.successful) {
      val opt = longOption.get
      getOptionDef(opt) match {
        case Some(optionDef) => if (optionDef.valueRequired) {
          matches.errors.append(s" option: `$str` requires an an arguments.")
        } else {
          matches.matches.append((optionDef, Given()))
        }
        case None =>
          matches.errors.append(opt)
      }
      return Option(opt)
    }
    None
  }

  private def getOptionDef(opt: String): Option[OptionDef] = {
    optDefinitions.find(_.equals(opt))
  }

  private object ParserImpl extends RegexParsers {
    def shortOptions: Parser[String] = "-[a-z]+".r ^^ {_.tail}
    def longOption: Parser[String] = "--[a-z\\-\\_]+".r ^^ {_.tail.tail}
    def valueOption: Parser[(String, String)] = "--[a-z\\-\\_]+=.+".r ^^ { e => {
      val s = e.tail.tail.split("=")
      (s.head, s.tail.mkString("="))
    }}
  }
}

object ArgsParser {
  protected[args_parser] case class OptionDef(shortName: Option[String], name: Option[String], usage: String, valueName: Option[String] = None) {
    val usageShortName: String = shortName.map(e => s"-$e").getOrElse("  ")
    val usageName: String = valueName match {
      case Some(value) => name.map(e => s"--$e=$value").getOrElse("")
      case None => name.map(e => s"--$e").getOrElse("")
    }

    val valueRequired: Boolean = valueName.isDefined

    override def equals(obj: scala.Any): Boolean = {
      obj match {
        case s: String => this.shortName.contains(s)  || this.name.contains(s)
        case o: OptionDef => o.shortName == this.shortName || o.name == this.name
        case _ => false
      }
    }

    def showUsage(padlength: Int): Unit = {
      val paddedName = usageName.padTo(padlength, ' ')
      if (shortName.isDefined && name.isDefined) {
        println(s"  $usageShortName, $paddedName  $usage")
      } else {
        println(s"  $usageShortName  $paddedName  $usage")
      }
    }
  }
}