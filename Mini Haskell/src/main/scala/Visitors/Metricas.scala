package Visitors

import Tipos._

class Metricas extends MHSVisitor[Int] {
  def visitar(e : Aplicacao): Int = 1 + e.args.map(e => e.aceitar(this)).sum
  def visitar(e : ExpressaoAnd): Int = 1 + e.lhs.aceitar(this) + e.rhs.aceitar(this)
  def visitar(e : ExpressaoFuncao): Int = 1 + e.argFuncao.map(e => e.aceitar(this)).sum + e.expFuncao.aceitar(this)
  def visitar(e : ExpressaoITE): Int = 1 + e.condicao.aceitar(this) + e.clausulaThen.aceitar(this)  + e.clausulaElse.aceitar(this)
  def visitar(e : ExpressaoLet): Int = 1 + e.expNomeada.aceitar(this) + e.corpo.aceitar(this)
  def visitar(e : ExpressaoNot): Int = 1 + e.exp.aceitar(this)
  def visitar(e : ExpressaoOr): Int = 1 + e.lhs.aceitar(this) + e.rhs.aceitar(this)
  def visitar(e : ExpressaoSoma): Int = 1 + e.lhs.aceitar(this) + e.rhs.aceitar(this)
  def visitar(e : Referencia): Int = 1
  def visitar(e : ValorBooleano): Int = 1
  def visitar(e: ValorErro): Int  = 0
  def visitar(e : ValorFuncao): Int = 1 + e.corpo._1.aceitar(this)
  def visitar(e : ValorInteiro): Int = 1
  def visitar(e : ValorNome): Int = 1
}
