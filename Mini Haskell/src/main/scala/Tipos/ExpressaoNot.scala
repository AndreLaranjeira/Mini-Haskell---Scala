package Tipos

import Visitors.MHSVisitor

class ExpressaoNot(val exp: Expressao) extends Expressao {

  override def avaliar() : Valor = {

    val v1 = exp.avaliar().asInstanceOf[ValorBooleano]

    ValorBooleano(!v1.valor)

  }

  override def aceitar[T](visitor : MHSVisitor[T]) : T =  visitor.visitar(this)

}
