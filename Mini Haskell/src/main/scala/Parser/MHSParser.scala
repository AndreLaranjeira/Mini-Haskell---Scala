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
      | number
      | boolean
  )

  def and : Parser[Expressao] = boolean~"and"~expressao ^^
    {case value1~"and"~value2 => new ExpressaoAnd(value1.asInstanceOf[Expressao], value2.asInstanceOf[Expressao])}

  def not : Parser[Expressao] = "not"~expressao ^^
  {case "not"~value => new ExpressaoNot(value.asInstanceOf[Expressao])}

  def or : Parser[Expressao] = boolean~"or"~expressao ^^
    {case value1~"or"~value2 =>
      new ExpressaoOr(value1.asInstanceOf[Expressao], value2.asInstanceOf[Expressao])}

  def sum : Parser[Expressao] = number~"+"~expressao ^^
    {case value1~"+"~value2 =>
      new ExpressaoSoma(value1.asInstanceOf[Expressao], value2.asInstanceOf[Expressao])}

  def boolean : Parser[Expressao] = "true" ^^ (x => ValorBooleano(true)) | "false" ^^ (x => ValorBooleano(false))

  def number : Parser[Expressao] = floatingPointNumber ^^ (number => ValorInteiro(number.toInt))

}
