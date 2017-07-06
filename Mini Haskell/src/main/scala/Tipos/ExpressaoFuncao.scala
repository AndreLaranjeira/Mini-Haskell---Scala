package Tipos

import Memoria.AmbienteExpressao
import Visitors.{MHSVisitor, VisitorTipo}


class ExpressaoFuncao(val expFuncao : ValorFuncao, val argFuncao : Expressao*) extends Expressao {
  override def avaliar(): Valor = {
    val visitor = new VisitorTipo
    if(visitor.visitar(this) != TErro) {
      AmbienteExpressao.novoEscopo()
      for (i <- argFuncao.indices){
        AmbienteExpressao.associar(expFuncao.arg(i)._1, argFuncao(i))
      }
      AmbienteExpressao.mudancaDeEscopo()
      expFuncao.corpo._1.avaliar()
    }
    else
      ValorErro(null)
  }

  override def aceitar[T](visitor: MHSVisitor[T]): T = visitor.visitar(this)
}