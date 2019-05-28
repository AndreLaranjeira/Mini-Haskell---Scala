package Tipos

import Visitors.{MHSVisitor, VisitorTipo}

class ExpressaoAnd(lhs : Expressao, rhs : Expressao) extends ExpressaoBinaria(lhs, rhs) {

  override def avaliar() : Valor = {

    val visitor = new VisitorTipo

    if(visitor.visitar(this) == TErro)
      ValorErro(null)

    else {
      val v1 = lhs.avaliar().asInstanceOf[ValorBooleano]
      val v2 = rhs.avaliar().asInstanceOf[ValorBooleano]

      ValorBooleano(v1.valor && v2.valor)
    }

  }

  override def aceitar[T](visitor : MHSVisitor[T]) : T =  visitor.visitar(this)

}
