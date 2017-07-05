package Tipos

import Visitors.{MHSVisitor, VisitorTipo}

class ExpressaoNot(val exp: Expressao) extends Expressao {

  override def avaliar() : Valor = {

    val visitor = new VisitorTipo

    if(visitor.visitar(this) == TErro)
      ValorErro(null)

    else {

      val v1 = exp.avaliar().asInstanceOf[ValorBooleano]

      ValorBooleano(!v1.valor)

    }

  }

  override def aceitar[T](visitor : MHSVisitor[T]) : T =  visitor.visitar(this)

}
