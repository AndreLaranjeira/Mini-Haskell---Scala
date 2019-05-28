import Tipos.{ExpressaoLet, ExpressaoSoma, Referencia, ValorInteiro}
import org.scalatest.{FlatSpec, Matchers}

/*
 * Classe de teste para expressoes do tipo Let
 */

class ExpressaoLetTeste extends FlatSpec with Matchers {

  //esse teste deve estar funcionando.
  "uma expressao let x = 3 in x + 1" should "levar ao valor 4" in {
    //val let = MHSParser.parse("let x = 3 in x + 1")
    val let = new ExpressaoLet("x",ValorInteiro(3),
      new ExpressaoSoma(new Referencia("x"),
        ValorInteiro(1)))

    let.avaliar().asInstanceOf[ValorInteiro].valor should be (4)

  }

  //esse teste deve estar funcionando. 
  "uma expressao let x = 3 in y = 5 in x + y" should "levar ao valor 8" in{
    val val3 = ValorInteiro(3)
    val val5 = ValorInteiro(5)
    val refY = new Referencia("y")
    val refX = new Referencia("x")
    val let1 = new ExpressaoLet("y", val5, new ExpressaoSoma(refX,refY))
    val let2 = new ExpressaoLet("x", val3, let1)

    let2.avaliar().asInstanceOf[ValorInteiro].valor should be (8)
  }

  //Esse caso de teste está funcionando.
  "uma expressao let x = 3 in (let x = 5 in x + 1) + x" should "levar ao valor 9" in {
    val val1 = ValorInteiro(1)
    val val3 = ValorInteiro(3)
    val val5 = ValorInteiro(5)
    val refX = new Referencia("x")
    val let1 = new ExpressaoLet("x", val5, new ExpressaoSoma(refX, val1))
    val let2 = new ExpressaoLet("x", val3, new ExpressaoSoma(let1, refX))

    let2.avaliar().asInstanceOf[ValorInteiro].valor should be (9)
  }

  //Para garantir que minha solução não é uma gambiarra, hora de testar os escopos de verdade.
  "uma expressao let y = 2 in (let x = 3 in (let x = 5 in (let y = 6 in (x + y))) + x + y)" should "levar ao valor 16" in {
    val val1 = ValorInteiro(1)
    val val2 = ValorInteiro(2)
    val val3 = ValorInteiro(3)
    val val5 = ValorInteiro(5)
    val val6 = ValorInteiro(6)
    val refX = new Referencia("x")
    val refY = new Referencia("y")
    val let1 = new ExpressaoLet("y", val6, new ExpressaoSoma(refX, refY))
    val let2 = new ExpressaoLet("x", val5, let1)
    val let3 = new ExpressaoLet("x", val3, new ExpressaoSoma(new ExpressaoSoma(let2, refX), refY))
    val let4 = new ExpressaoLet("y", val2, let3)

    let4.avaliar().asInstanceOf[ValorInteiro].valor should be (16)
  }

}