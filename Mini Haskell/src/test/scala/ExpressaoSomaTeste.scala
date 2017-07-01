import Tipos.{ExpressaoSoma, ValorInteiro}
import org.scalatest.{FlatSpec, Matchers}

class ExpressaoSomaTeste extends FlatSpec with Matchers {

  "Uma soma entre os valores 3 e 4" should "levar ao valor sete" in {

    val soma = new ExpressaoSoma(ValorInteiro(3), ValorInteiro(4))
    soma.avaliar().asInstanceOf[ValorInteiro].valor should be (7)

  }


}