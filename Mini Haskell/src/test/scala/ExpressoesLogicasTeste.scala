import Tipos._
import org.scalatest.{FlatSpec, Matchers}

class ExpressoesLogicasTeste extends FlatSpec with Matchers {

  "true && false " should " result in false" in {

    val logic = new ExpressaoAnd(ValorBooleano(true), ValorBooleano(false))

    logic.avaliar().asInstanceOf[ValorBooleano].valor should be (false)

  }

  "true || false " should " result in true" in {

    val logic = new ExpressaoOr(ValorBooleano(true), ValorBooleano(false))

    logic.avaliar().asInstanceOf[ValorBooleano].valor should be (true)

  }

  "!false " should " result in true" in {

    val logic = new ExpressaoNot(ValorBooleano(false))

    logic.avaliar().asInstanceOf[ValorBooleano].valor should be (true)

  }

  "!((true || false) && false) " should " result in true" in {

    val sublogic1 = new ExpressaoOr(ValorBooleano(true), ValorBooleano(false))
    val sublogic2 = new ExpressaoAnd(sublogic1, ValorBooleano(false))
    val logic = new ExpressaoNot(sublogic2)

    logic.avaliar().asInstanceOf[ValorBooleano].valor should be (true)

  }

  "5 && true" should " result in ValorErro" in {

    val logic = new ExpressaoAnd(ValorInteiro(5), ValorBooleano(true))

    logic.avaliar() should be (ValorErro(null))

  }

  "5 || true" should " result in ValorErro" in {

    val logic = new ExpressaoOr(ValorInteiro(5), ValorBooleano(true))

    logic.avaliar() should be (ValorErro(null))

  }

  "!5" should " result in ValorErro" in {

    val logic = new ExpressaoNot(ValorInteiro(5))

    logic.avaliar() should be (ValorErro(null))

  }

  "if(true) 5 else 5 && true" should " result in ValorErro" in {

    val sublogic = new ExpressaoITE(ValorBooleano(true), ValorInteiro(5), ValorInteiro(5))
    val logic = new ExpressaoAnd(ValorInteiro(5), ValorBooleano(true))

    logic.avaliar() should be (ValorErro(null))

  }

}