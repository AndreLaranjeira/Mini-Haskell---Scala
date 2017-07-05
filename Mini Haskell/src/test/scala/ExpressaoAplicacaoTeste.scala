import Memoria.AmbienteDecFuncao
import Tipos._
import org.scalatest.{FlatSpec, Matchers}

class ExpressaoAplicacaoTeste extends FlatSpec with Matchers {

  "uma aplicacao inc 4 (onde existe inc x = x + 1) " should " levar ao valor 5" in {

    val inc = new DecFuncao("inc", List(("x", TInteiro)), (new ExpressaoSoma(new Referencia("x"), ValorInteiro(1)), TInteiro))
    val app = new Aplicacao("inc", ValorInteiro(4))

    AmbienteDecFuncao.associar("inc", inc)

    app.avaliar().asInstanceOf[ValorInteiro].valor should be (5)

  }

  //TODO: isso eh uma aberracao, pois tem a semantica de scopo dinamico. 
  "supondo (def f y = x + y), e avaliamos let x = 10 in f 5 " should " levar ao 15" in {

    val refX = new Referencia("x")
    val f    = new DecFuncao("f", List(("y", TInteiro)), (new ExpressaoSoma(refX, new Referencia("y")), TInteiro))
    val let  = new ExpressaoLet("x", ValorInteiro(10), new Aplicacao("f", ValorInteiro(5)))

    AmbienteDecFuncao.associar("f", f)

    let.avaliar().asInstanceOf[ValorInteiro].valor should be (15)

  }

}