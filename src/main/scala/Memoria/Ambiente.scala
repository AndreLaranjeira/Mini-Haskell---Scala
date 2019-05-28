package Memoria

import Tipos.{DecFuncao, Expressao}
import scala.collection.mutable

/**
  * Um ambiente para associar identificadores a expressoes.
  * Essencial na avaliacao de expressoes como Let e avaliacao de
  * funcoes (ainda nao implementadas).
  */

class Ambiente[T] {

  private var contexto : mutable.HashMap[String, T] = new mutable.HashMap()
  private var escopos: List[mutable.HashMap[String, T]] = List()

  /**
    * Associa um id a uma expressao
    */

  def associar(id : String, exp : T): Unit = {
    contexto += id -> exp
  }

  def mudancaDeEscopo(): Unit = {
    contexto = escopos.head
    escopos = escopos.drop(1)
  }

  /**
    * Cria um novo escopo para variáveis.
    */

  def novoEscopo(): Unit = {

    escopos = new mutable.HashMap[String, T]() :: escopos

    for(pair <- contexto)
      escopos.head += pair

  }

  /**
    * Retorna a expressao associada a um id
    */

  def pesquisar(id : String) : T = contexto(id)

}

object AmbienteExpressao extends Ambiente[Expressao]
object AmbienteDecFuncao extends Ambiente[DecFuncao]
