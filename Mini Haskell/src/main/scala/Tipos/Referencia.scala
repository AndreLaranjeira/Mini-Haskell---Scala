package Tipos

import Memoria.AmbienteExpressao
import Visitors.{MHSVisitor, VisitorTipo}

class Referencia(val id: String) extends Expressao {

  override def avaliar() : Valor = {
    val visitor = new VisitorTipo
    if(visitor.visitar(this) != TErro)
      AmbienteExpressao.pesquisar(id).avaliar()
    else
      ValorErro
  }

  /*override def verificarTipo() : Tipo = {

    val exp = AmbienteExpressao.pesquisar(id)

    if(exp != null)
      exp.verificarTipo()
    else
      TErro

  }
  */

  override def aceitar[T](visitor : MHSVisitor[T]) : T =  visitor.visitar(this)

}
