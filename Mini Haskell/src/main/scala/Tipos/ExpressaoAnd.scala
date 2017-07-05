package Tipos

import Visitors.MHSVisitor

class ExpressaoAnd(lhs : Expressao, rhs : Expressao) extends ExpressaoBinaria(lhs, rhs) {

  override def avaliar() : Valor = {

    val v1 = lhs.avaliar().asInstanceOf[ValorBooleano]
    val v2 = rhs.avaliar().asInstanceOf[ValorBooleano]

    ValorBooleano(v1.valor && v2.valor)

  }

  override def aceitar[T](visitor : MHSVisitor[T]) : T =  visitor.visitar(this)

}
