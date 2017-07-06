package Visitors

import Memoria.{Ambiente, AmbienteDecFuncao, AmbienteExpressao}
import Tipos._

class VisitorTipo extends MHSVisitor[Tipo]{

  override def visitar(e: Aplicacao): Tipo = {
    val funcao = AmbienteDecFuncao.pesquisar(e.nome)
    var tiposArgumentos: List[Tipo] = List()

    AmbienteExpressao.novoEscopo()

    for(i <- funcao.args.indices){
      AmbienteExpressao.associar(funcao.args(i)._1, e.args(i))
    }

    val tipoRetorno = funcao.corpo._1.aceitar(this)

    AmbienteExpressao.mudancaDeEscopo()

    if(tipoRetorno == TErro || e.args.size != funcao.args.size)
      TErro

    else {

      for(i <- funcao.args.indices) {
        if(funcao.args(i)._2 != e.args(i).aceitar(this) || funcao.args(i)._2 == TErro)
          TErro
      }

      tipoRetorno

    }

  }

  override def visitar(e: ExpressaoAnd): Tipo = {

    val lhs = e.lhs.aceitar(this)
    val rhs = e.rhs.aceitar(this)

    if(lhs == TBooleano && rhs == TBooleano)
      TBooleano

    else
      TErro

  }

  override def visitar(e: ExpressaoITE): Tipo = {

    val tipoCondicao = e.condicao.aceitar(this)
    val tipoThen = e.clausulaThen.aceitar(this)
    val tipoElse = e.clausulaThen.aceitar(this)

    if(tipoCondicao != TBooleano || tipoThen != tipoElse)
      TErro

    else {
      tipoThen
    }

  }

  override def visitar(e: ExpressaoLet): Tipo = {

    AmbienteExpressao.novoEscopo()

    AmbienteExpressao.associar(e.id, e.expNomeada)

    val expAceitar = e.expNomeada.aceitar(this)
    val corpoAceitar = e.corpo.aceitar(this)

    if(expAceitar != TErro && corpoAceitar != TErro) {
      AmbienteExpressao.mudancaDeEscopo()
      corpoAceitar
    }

    else {
      AmbienteExpressao.mudancaDeEscopo()
      TErro
    }

  }

  override def visitar(e: ExpressaoNot): Tipo = {

    val exp = e.exp.aceitar(this)

    if(exp == TBooleano)
      TBooleano

    else
      TErro

  }

  override def visitar(e: ExpressaoOr): Tipo = {

    val lhs = e.lhs.aceitar(this)
    val rhs = e.rhs.aceitar(this)

    if(lhs == TBooleano && rhs == TBooleano)
      TBooleano

    else
      TErro

  }

  override def visitar(e: ExpressaoSoma): Tipo = {

    val lhs = e.lhs.aceitar(this)
    val rhs = e.rhs.aceitar(this)

    if(lhs == TInteiro && rhs == TInteiro)
      TInteiro

    else
      TErro

  }

  override def visitar(e: Referencia): Tipo = {

    val exp = AmbienteExpressao.pesquisar(e.id)

    if(exp != null)
      exp.aceitar(this)

    else
      TErro

  }

  override def visitar(e: ValorBooleano): Tipo = TBooleano

  override def visitar(e: ValorErro): Tipo = TErro

  override def visitar(e: ValorInteiro): Tipo = TInteiro

  override def visitar(e: ValorNome): Tipo = TNome

}
