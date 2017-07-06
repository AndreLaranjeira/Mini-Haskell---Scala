package Parser

import Tipos._

import scala.util.parsing.combinator.JavaTokenParsers

class MHSParser extends JavaTokenParsers {

  def mhs : Parser[Expressao] = expressao

  def expressao : Parser[Expressao] = (
    not
      | and
      | or
      | sum
      | boolean
      | number
      | string
  )

  def and : Parser[Expressao] = (boolean~"and"~expressao ^^
    {case value1~"and"~value2 => new ExpressaoAnd(value1.asInstanceOf[Expressao], value2.asInstanceOf[Expressao])}
  | number~"and"~expressao ^^ (x => ValorErro(null)))

  def not : Parser[Expressao] = "not"~expressao ^^
  {case "not"~value => new ExpressaoNot(value.asInstanceOf[Expressao])
  case _ => ValorErro(null)}

  def or : Parser[Expressao] = (boolean~"or"~expressao ^^
    {case value1~"or"~value2 => new ExpressaoOr(value1.asInstanceOf[Expressao], value2.asInstanceOf[Expressao])}
    | number~"or"~expressao ^^ (x => ValorErro(null)))

  def sum : Parser[Expressao] = (number~"+"~expressao ^^
    {case value1~"+"~value2 => new ExpressaoSoma(value1.asInstanceOf[Expressao], value2.asInstanceOf[Expressao])}
    | boolean~"+"~expressao ^^ (x => ValorErro(null)))

  def boolean : Parser[Expressao] = "true" ^^ (x => ValorBooleano(true)) | "false" ^^ (x => ValorBooleano(false))

  def number : Parser[Expressao] = floatingPointNumber ^^ (number => ValorInteiro(number.toInt))

  def string : Parser[Expressao] = stringLiteral ^^ (text => ValorNome(text))

}
