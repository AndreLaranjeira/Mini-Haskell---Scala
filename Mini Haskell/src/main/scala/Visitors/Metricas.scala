package Visitors

import Tipos._

class Metricas extends MHSVisitor[Int] {
  def visitar(e : ValorInteiro): Int = 1
  def visitar(e : ValorBooleano): Int = 1
  def visitar(e : ExpressaoSoma): Int = 1 + e.lhs.aceitar(this) + e.rhs.aceitar(this)
  def visitar(e : ExpressaoITE): Int = 1 + e.condicao.aceitar(this) + e.clausulaThen.aceitar(this)  + e.clausulaElse.aceitar(this)
  def visitar(e : Aplicacao): Int = 1 + e.args.map(e => e.aceitar(this)).sum
  def visitar(e : ExpressaoLet): Int = 1 + e.expNomeada.aceitar(this) + e.corpo.aceitar(this)
  def visitar(e : Referencia): Int = 1
}
