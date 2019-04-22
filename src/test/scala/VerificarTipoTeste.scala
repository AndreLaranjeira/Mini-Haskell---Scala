import Memoria._
import Tipos._
import Visitors.VisitorTipo
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by victo on 06/07/2017.
  */
class VerificarTipoTeste extends FlatSpec with Matchers{

  "verificar tipo de ValorInteiro " should " levar ao tipo TInteiro" in {

    val valorInteiro = ValorInteiro(3)
    val visitor = new VisitorTipo

    valorInteiro.aceitar(visitor) should be (TInteiro)

  }

  "verificar tipo (1 + 2) + 3 " should "levar ao tipo TInteiro" in {

    val soma = new ExpressaoSoma(new ExpressaoSoma(ValorInteiro(1),ValorInteiro(2)), ValorInteiro(3))
    val visitor = new VisitorTipo

    soma.aceitar(visitor) should be (TInteiro)

  }

  "verificar tipo da aplicação inc 4 (onde existe inc x = x + 1) " should " levar ao tipo TInteiro" in {

    val inc = new DecFuncao("inc", List(("x", TInteiro)), (new ExpressaoSoma(new Referencia("x"), ValorInteiro(1)), TInteiro))
    val app = new Aplicacao("inc", ValorInteiro(4))

    val visitor = new VisitorTipo

    app.aceitar(visitor) should be (TInteiro)

  }

  "verificar tipo de true and false " should " levar ao tipo TBoolean" in {

    val and = new ExpressaoAnd(ValorBooleano(true), ValorBooleano(false))

    val visitor = new VisitorTipo

    and.aceitar(visitor) should be (TBooleano)

  }

  "verificar tipo de true or false " should " levar ao tipo TBoolean" in {

    val or = new ExpressaoOr(ValorBooleano(true), ValorBooleano(false))

    val visitor = new VisitorTipo

    or.aceitar(visitor) should be (TBooleano)

  }

  "verificar tipo de not true " should " levar ao tipo TBooleano" in {

    val not = new ExpressaoNot(ValorBooleano(false))

    val visitor = new VisitorTipo

    not.aceitar(visitor) should be (TBooleano)

  }

  "verificar tipo de Referencia(\"x\") dado x = 6 " should " levar ao tipo TInteiro" in {

    val refX = new Referencia("x")
    AmbienteExpressao.novoEscopo()
    AmbienteExpressao.associar("x", ValorInteiro(6))

    refX.aceitar(new VisitorTipo) should be (TInteiro)

  }

  "verificar tipo de let x = 10 in x + 2 " should " levar ao tipo TInteiro" in {

    val let = new ExpressaoLet("x", ValorInteiro(10), new ExpressaoSoma(new Referencia("x"), ValorInteiro(2)))

    let.aceitar(new VisitorTipo) should be (TInteiro)

  }

  "verificar tipo de if (false) 5 else 10 " should " levar ao tipo TInteiro" in {

    val ite = new ExpressaoITE(ValorBooleano(false), ValorInteiro(5), ValorInteiro(10))

    ite.aceitar(new VisitorTipo) should be (TInteiro)

  }

  "verificar tipo de if (false) true else 5 " should " levar ao tipo TErro" in {

    val ite = new ExpressaoITE(ValorBooleano(false), ValorBooleano(true), ValorInteiro(5))

    ite.aceitar(new VisitorTipo) should be (TErro)

  }

}
