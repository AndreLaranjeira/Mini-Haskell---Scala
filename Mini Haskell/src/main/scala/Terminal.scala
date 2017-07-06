import Parser.MHSParser
import Tipos._

object Terminal extends App {

  var Input = " "
  val parser = new MHSParser
  var parserResult: Expressao = _

  while(Input != "quit") {

    print("[bonifacio@mhs]>")
    Input = scala.io.StdIn.readLine()

    if(Input != "quit") {

      parser.parseAll(parser.mhs, Input) match {
        case parser.Success(result, _) => parserResult = result.asInstanceOf[Expressao]
        case parser.NoSuccess(_, _) => parserResult = ValorErro(null)
      }

      println(parserResult.avaliar())
      println()

    }

  }

}
