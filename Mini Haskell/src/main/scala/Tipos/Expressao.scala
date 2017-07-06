package Tipos

import Visitors.MHSVisitor

class Tipo

case object TBooleano extends Tipo
case object TErro extends Tipo
case object TInteiro extends Tipo
case object TNome extends Tipo

trait Expressao {
  def avaliar() : Valor
  //def verificarTipo() : Tipo
  def aceitar[T](visitor : MHSVisitor[T]) : T
}
