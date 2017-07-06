package Tipos

import Memoria.{AmbienteDecFuncao, AmbienteExpressao}
import Visitors.MHSVisitor
import Visitors.VisitorTipo

class Aplicacao (val nome: String, val args: Expressao*) extends Expressao{

  override def avaliar(): Valor = {
    val visitor = new VisitorTipo
    val funcao = AmbienteDecFuncao.pesquisar(nome)

    AmbienteExpressao.novoEscopo()

    if(visitor.visitar(this) != TErro) {
      for (i <- funcao.args.indices) {
        AmbienteExpressao.associar(funcao.args(i)._1, args(i))
      }

      val resultado = funcao.corpo._1.avaliar()

      AmbienteExpressao.mudancaDeEscopo()

      resultado
    }
    else
      ValorErro

  }

  /*override def verificarTipo() : Tipo = {

    val funcao = AmbienteDecFuncao.pesquisar(nome)
    var tiposArgumentos: List[Tipo] = List()

    for(i <- funcao.args.indices){
      AmbienteExpressao.associar(funcao.args(i)._1, this.args(i))
    }

    val tipoRetorno = funcao.corpo._1.verificarTipo()

    if(tipoRetorno == TErro || tipoRetorno != funcao.corpo._2 || this.args.size != funcao.args.size)
      TErro

    else {

      for(i <- funcao.args.indices) {

        if(funcao.args(i)._2 == this.args(i).verificarTipo() && funcao.args(i)._2 != TErro)
          tiposArgumentos = funcao.args(i)._2 :: tiposArgumentos

        else
          TErro

      }

      TFuncao(tiposArgumentos, tipoRetorno)

    }

  }
  */

  override def aceitar[T](visitor : MHSVisitor[T]) : T =  visitor.visitar(this)

}
