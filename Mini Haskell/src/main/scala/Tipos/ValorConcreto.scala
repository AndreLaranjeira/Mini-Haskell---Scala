package Tipos

import Visitors.MHSVisitor

abstract class ValorConcreto[T](val valor : T) extends Valor {
  override def avaliar() : Valor = this
}

case class ValorBooleano(v : Boolean) extends ValorConcreto[Boolean](v) {
  //override def verificarTipo() : Tipo = TBooleano
  override def aceitar[T](visitor : MHSVisitor[T]) : T = visitor.visitar(this)
}

case class ValorErro(v : Null) extends ValorConcreto[Null](v) {
  override def aceitar[T](visitor: MHSVisitor[T]): T = visitor.visitar(this)
  override def avaliar() : Valor = null
}

case class ValorInteiro(v : Int) extends ValorConcreto[Int](v) {
  //override def verificarTipo() : Tipo = TInteiro
  override def aceitar[T](visitor : MHSVisitor[T]) : T = visitor.visitar(this)
}
