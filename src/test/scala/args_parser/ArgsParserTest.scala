package args_parser

import org.scalatest.{FlatSpec, Matchers}

class ArgsParserTest extends FlatSpec with Matchers {
  val parser = new ArgsParser
  parser.add("h", "help", "show usage")
  parser.add("s", "", "short option only")
  parser.add("", "long", "long option")
  parser.add("", "page-size=SIZE", "value")

  parser.showUsage()

  "short option" should "run right" in {
    parser.parse(Array("-h")) match {
      case Right(matches) =>
        matches.frees.size should be (0)
        matches.present("h") should be (true)
        matches.present("help") should be (true)
        matches.value("h") should be (None)
        matches.present("s") should be (false)
        matches.present("long") should be (false)
        matches.present("page-size") should be (false)
      case Left(_) =>
        fail("unmatch option")
    }
  }

  "many short option" should "run right" in {
    parser.parse(Array("-hs")) match {
      case Right(matches) =>
        matches.frees.size should be (0)
        matches.present("h") should be (true)
        matches.present("help") should be (true)
        matches.value("h") should be (None)
        matches.present("s") should be (true)
        matches.present("long") should be (false)
        matches.present("page-size") should be (false)
      case Left(_) =>
        fail("unmatch option")
    }
  }

  "long option" should "run right" in {
    parser.parse(Array("--help")) match {
      case Right(matches) =>
        matches.frees.size should be (0)
        matches.present("h") should be (true)
        matches.present("help") should be (true)
        matches.value("h") should be (None)
        matches.present("s") should be (false)
        matches.present("long") should be (false)
        matches.present("page-size") should be (false)
      case Left(_) =>
        fail("unmatch option")
    }
  }

  "value option" should "run right" in {
    parser.parse(Array("--page-size=100")) match {
      case Right(matches) =>
        matches.frees.size should be (0)
        matches.present("page-size") should be (true)
        matches.present("help") should be (false)
        matches.present("long") should be (false)
        matches.value("page-size") match {
          case Some(v) => v should be ("100")
          case None => fail("failed to get Value")
        }
      case Left(_) =>
        fail("unmatch option")
    }
  }

  "invalid option" should "run right" in {
    parser.parse(Array("--invalid-option")) match {
      case Right(_) => fail("unexpected parse option")
      case Left(_) =>
    }
    parser.parse(Array("-i")) match {
      case Right(_) => fail("unexpected parse option")
      case Left(_) =>
    }
    parser.parse(Array("--page-size")) match {
      case Right(_) => fail("unexpected parse option, required value")
      case Left(_) =>
    }
  }

  "frees" should "run right" in {
    parser.parse(Array("--page-size=500", "hoge","fuga")) match {
      case Right(matches) =>
        matches.frees.size should be (2)
        matches.frees.contains("hoge") should be (true)
        matches.frees.contains("fuga") should be (true)
        matches.present("page-size") should be (true)
        matches.present("help") should be (false)
        matches.present("long") should be (false)
        matches.value("page-size") match {
          case Some(v) => v should be ("500")
          case None => fail("failed to get Value")
        }
      case Left(_) =>
        fail("unmatch option")
    }
  }
}
