package Tipos

import Memoria.{AmbienteDecFuncao, AmbienteExpressao}
import Visitors.MHSVisitor

class Aplicacao (val nome: String, val args: Expressao*) extends Expressao{

  override def avaliar(): Valor = {

    val funcao = AmbienteDecFuncao.pesquisar(nome)

    for(i <- funcao.args.indices){
      AmbienteExpressao.associar(funcao.args(i), args(i))
    }

    funcao.corpo.avaliar()

  }

  override def verificarTipo() : Tipo = TErro

  override def aceitar[T](visitor : MHSVisitor[T]) : T =  visitor.visitar(this)

}
