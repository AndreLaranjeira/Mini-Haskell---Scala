package Visitors
import Tipos._

class TransformacaoLambda extends TransformacaoG {

  val visitor = new VisitorTipo

  override def visitar(e: ExpressaoLet): Expressao = {
    new ExpressaoFuncao(ValorFuncao((e.corpo.aceitar(this), e.corpo.aceitar(visitor)), List((e.id, TString))), e.expNomeada)
  }
}
