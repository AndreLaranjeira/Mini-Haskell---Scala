package Tipos

import Visitors.MHSVisitor

class ExpressaoSoma(lhs : Expressao, rhs : Expressao) extends ExpressaoBinaria(lhs, rhs) {

  /* essa implementacao eh fragil, pois nao verifica os tipos */

  override def avaliar() : Valor = {
    val v1 = lhs.avaliar().asInstanceOf[ValorInteiro]
    val v2 = rhs.avaliar().asInstanceOf[ValorInteiro]

    ValorInteiro(v1.valor + v2.valor)

  }

  override def verificarTipo() : Tipo = {
    val t1 = lhs.verificarTipo()
    val t2 = rhs.verificarTipo()

    if(t1.equals(TInteiro) && t2.equals(TInteiro)) TInteiro else TErro

  }

  override def aceitar[T](visitor : MHSVisitor[T]) : T =  visitor.visitar(this)

}
