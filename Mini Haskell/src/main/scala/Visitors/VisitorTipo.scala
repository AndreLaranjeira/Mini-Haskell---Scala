package Visitors

import Memoria.{Ambiente, AmbienteDecFuncao, AmbienteExpressao}
import Tipos._

class VisitorTipo extends MHSVisitor[Tipo]{

  override def visitar(e: ExpressaoAnd): Tipo = {
    if(e.lhs.aceitar(this) == TBooleano && e.rhs.aceitar(this) == TBooleano)
      TBooleano
    else
      TErro
  }

  override def visitar(e: ExpressaoNot): Tipo = {
    if(e.exp.aceitar(this) == TBooleano)
      TBooleano
    else
      TErro
  }

  override def visitar(e: ExpressaoOr): Tipo = {
    if(e.lhs.aceitar(this) == TBooleano && e.rhs.aceitar(this) == TBooleano)
      TBooleano
    else
      TErro
  }

  override def visitar(e: ValorInteiro): Tipo = TInteiro

  override def visitar(e: ValorBooleano): Tipo = TBooleano

  override def visitar(e: ExpressaoSoma): Tipo = {
    if(e.lhs.aceitar(this) == e.rhs.aceitar(this))
      TInteiro
    else
      TErro
  }

  override def visitar(e: ExpressaoITE): Tipo = {
    if(e.condicao.aceitar(this) != TBooleano || e.clausulaThen.aceitar(this) != e.clausulaElse.aceitar(this))
      TErro
    else
      e.condicao.aceitar(this)
  }

  override def visitar(e: Aplicacao): Tipo = {
    val funcao = AmbienteDecFuncao.pesquisar(e.nome)
    var tiposArgumentos: List[Tipo] = List()

    for(i <- funcao.args.indices){
      AmbienteExpressao.associar(funcao.args(i)._1, e.args(i))
    }
    val tipoRetorno = funcao.corpo._1.aceitar(this)

    if(tipoRetorno == TErro || e.args.size != funcao.args.size)
      TErro

    else {
      for(i <- funcao.args.indices) {
        if(funcao.args(i)._2 == e.args(i).aceitar(this) && funcao.args(i)._2 != TErro)
          tiposArgumentos = funcao.args(i)._2 :: tiposArgumentos
        else
          TErro
      }
      TFuncao(tiposArgumentos, tipoRetorno)
    }
  }

  override def visitar(e: ExpressaoLet): Tipo = TErro

  override def visitar(e: Referencia): Tipo = TErro
}
