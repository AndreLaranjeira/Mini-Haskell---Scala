import Tipos.{ExpressaoSoma, ValorInteiro}
import Visitors.Metricas
import org.scalatest.{FlatSpec, Matchers}

class MetricasTeste extends FlatSpec with Matchers {

  "o total de expressoes 3 + (4 + 5)" should "levar ao valor cinco" in {

    val soma1 = new ExpressaoSoma(ValorInteiro(4), ValorInteiro(5))
    val soma2 = new ExpressaoSoma(ValorInteiro(3), soma1)

    val m : Metricas = new Metricas()
    soma2.aceitar(m) should be (5)

  }

}