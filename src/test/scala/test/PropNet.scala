package test

import scala.util.Random

/**
 * @author eecolor
 */
class PropNet {

  trait Node {
    def value: Boolean
  }
 
  case class Base(value:Boolean) extends Node

  case class Input(input: () => Boolean) extends Node {
    def value = input()
  }

  case class ViewNode(a: Node) extends Node {
    def value = a.value
  }

  case class Or(a: Node, b: Node) extends Node {
    def value = a.value || b.value
  }

  case class Not(a: Node) extends Node {
    def value = !a.value
  }

  case class And(a: Node, b: Node) extends Node {
    def value = a.value && b.value
  }

  case class Goal(a:Node, value:Int) {
    def score = if (a.value) value else 0 
  } 
}