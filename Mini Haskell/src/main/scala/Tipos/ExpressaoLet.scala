package Tipos

import Memoria.AmbienteExpressao
import Visitors.{MHSVisitor, VisitorTipo}

/**
  * Classe que representa uma expressao do tipo Let.
  * Permite escrever algo como let x = 10 in x + 1, o que levaria
  * ao valor 11.
  */

class ExpressaoLet(val id : String , val expNomeada: Expressao , val corpo: Expressao) extends Expressao {

  override def avaliar() : Valor = {
    val visitor = new VisitorTipo
    AmbienteExpressao.novoEscopo()
    AmbienteExpressao.associar(id, expNomeada)
    if(visitor.visitar(this) != TErro) {
      val resultado = corpo.avaliar()
      AmbienteExpressao.mudancaDeEscopo()
      resultado
    }
    else
      ValorErro(null)
  }

  //override def verificarTipo() : Tipo = if(expNomeada.verificarTipo().equals(TErro)) TErro else  corpo.verificarTipo()

  override def aceitar[T](visitor : MHSVisitor[T]) : T = visitor.visitar(this)

}