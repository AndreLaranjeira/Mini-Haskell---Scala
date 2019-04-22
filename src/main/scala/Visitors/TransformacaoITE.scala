package Visitors

import Tipos._

class TransformacaoITE extends TransformacaoG {
  override def visitar(e : ExpressaoITE): Expressao = {

    val condicao = e.condicao.aceitar(this)
    val tupla = (e.clausulaThen, e.clausulaElse)

    tupla match {
      case (ValorBooleano(true), ValorBooleano(false)) => condicao
      case _ => new ExpressaoITE(condicao, e.clausulaThen.aceitar(this), e.clausulaElse.aceitar(this))
    }

  }
}
