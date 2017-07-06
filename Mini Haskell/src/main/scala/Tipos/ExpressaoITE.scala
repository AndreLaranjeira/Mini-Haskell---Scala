package Tipos

import Visitors.{MHSVisitor, VisitorTipo}

/**
  * Epressao If-Then-Else, permitindo algo como:
  *
  * 4 + (If cond Then 5 Else 10)
  */

class ExpressaoITE(val condicao: Expressao, val clausulaThen: Expressao, val clausulaElse: Expressao) extends Expressao{

  override def avaliar() : Valor = {
    val visitor = new VisitorTipo()
    if (visitor.visitar(this) != TErro){
      if (condicao.avaliar().asInstanceOf[ValorBooleano].valor) clausulaThen.avaliar()
      else clausulaElse.avaliar()
    }
    else
      ValorErro
  }
  /*override def verificarTipo() : Tipo =

    if(condicao.verificarTipo() == TBooleano && clausulaThen.verificarTipo() == clausulaElse.verificarTipo())
      clausulaThen.verificarTipo()
    else
      TErro
  */
  override def aceitar[T](visitor : MHSVisitor[T]) : T =  visitor.visitar(this)

}