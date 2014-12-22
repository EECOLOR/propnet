package test

import org.qirx.littlespec.Specification
import scala.language.implicitConversions

trait Keywords {
  case class Role(constant: Constant)
  case class Init(f: Application[Fluent])

  trait Parameter
  case class Constant(name: String) extends Parameter
  case class Variable(name: String) extends Parameter
  case class Score(value: Int)

  case class Move(name: String, arity: Int) extends DefaultApplication[Move]

  case class Application[A <: Applicable](a: A, parameters: Seq[Parameter]) {
    require(parameters.size == a.arity)
  }

  trait Applicable {
    type ApplicationType

    def arity: Int

    def apply(parameters: Parameter*): ApplicationType
  }

  trait DefaultApplication[T <: DefaultApplication[T]] extends Applicable { self: T =>
    type ApplicationType = Application[T]

    def apply(parameters: Parameter*) = Application(self, parameters)
  }

  case class Fluent(name: String, arity: Int) extends DefaultApplication[Fluent]
  case class Function(name: String, arity: Int) extends Applicable {
    type ApplicationType = Application[Function] with Predicate
    def apply(parameters: Parameter*) =
      new Application[Function](this, parameters) with Predicate
  }

  case class Does(r: Parameter, m: Application[Move]) extends Predicate
  case class Distinct(p1: Parameter, p2: Parameter) extends Predicate
  case class True(a: Application[Fluent]) extends Predicate

  case class Next(f: Application[Fluent], d: Predicate)
  case class Terminal(p: Predicate)

  case class Relation(f: Application[Function], p: Predicate)

  trait Predicate

  case class Legal(r: Parameter, m: Application[Move], p: Predicate)
  case class And(left: Predicate, right: Predicate) extends Predicate
  case class Or(left: Predicate, right: Predicate) extends Predicate
  case class Not(p: Predicate) extends Predicate

  case class Goal(r: Parameter, s: Score, p: Predicate)
}

object Start extends Specification with Keywords {

  "Do it!" - {

    val xplayer = Constant("xplayer")
    val oplayer = Constant("oplayer")

    val cell = Fluent("cell", 3)
    val mark = Move("mark", 2)
    val control = Fluent("control", 1)
    val noop = Move("noop", 0)

    val x = Constant("x")
    val o = Constant("o")
    val b = Constant("b")

    val X = Variable("X")
    val Y = Variable("Y")

    val P = Variable("P")

    val `1` = Constant("1")
    val `2` = Constant("2")
    val `3` = Constant("3")

    val M = Variable("M")
    val N = Variable("N")
    val W = Variable("W")
    val J = Variable("J")
    val K = Variable("K")

    val line = Function("line", 1)

    val row = Function("row", 2)
    val column = Function("column", 2)
    val diagonal = Function("diagonal", 1)
    val open = Function("open", 0)

    /*
     * Round => Option[Round]
     * 
     *  Player("X", score, availableMoves:Seq[Move(state)]), 
     *  Player("Y", score, availableMoves),
     *  States(Seq[State(name, value)])
     *  
     * round  
     *  select move
     *  apply move to state
     *  if (isTerminated) None
     *  else {
     *    calculate available moves
     *    Some(next round)
     *  }
     */
     
    Role(xplayer)
    Role(oplayer)

    Init(cell(`1`, `1`, b))
    Init(cell(`1`, `2`, b))
    Init(cell(`1`, `3`, b))
    Init(cell(`2`, `1`, b))
    Init(cell(`2`, `2`, b))
    Init(cell(`2`, `3`, b))
    Init(cell(`3`, `1`, b))
    Init(cell(`3`, `2`, b))
    Init(cell(`3`, `3`, b))
    Init(control(xplayer))

    Legal(P, mark(X, Y),
      And(True(cell(X, Y, b)), True(control(P))))
    Legal(xplayer, noop(), True(control(oplayer)))
    Legal(oplayer, noop(), True(control(xplayer)))

    Next(cell(M, N, x), Does(xplayer, mark(M, N)))
    Next(cell(M, N, o), Does(oplayer, mark(M, N)))

    Next(cell(M, N, W),
      And(True(cell(M, N, W)), Distinct(W, b)))

    Next(cell(M, N, b), And(
      True(cell(M, N, b)),
      And(Does(P, mark(J, K)),
        Or(Distinct(M, J), Distinct(N, K)))))

    Next(control(xplayer), True(control(oplayer)))
    Next(control(oplayer), True(control(xplayer)))

    Terminal(And(line(x), line(o)))
    Terminal(Not(open()))

    Relation(line(W), row(M, W))
    Relation(line(W), column(M, W))
    Relation(line(W), diagonal(W))

    Relation(open(), True(cell(M, N, b)))

    Relation(row(M, W), And(True(cell(M, `1`, W)), And(True(cell(M, `2`, W)), True(cell(M, `3`, W)))))
    Relation(column(N, W), And(True(cell(`1`, N, W)), And(True(cell(`2`, N, W)), True(cell(`3`, N, W)))))
    Relation(diagonal(W), And(True(cell(`1`, `1`, W)), And(True(cell(`2`, `2`, W)), True(cell(`3`, `3`, W)))))
    Relation(diagonal(W), And(True(cell(`1`, `3`, W)), And(True(cell(`2`, `2`, W)), True(cell(`3`, `1`, W)))))

    Goal(xplayer, Score(100), line(x))
    Goal(xplayer, Score(50), And(Not(line(x)), And(Not(line(o)), Not(open()))))
    Goal(xplayer, Score(0), line(o))

    Goal(oplayer, Score(100), line(o))
    Goal(oplayer, Score(50), And(Not(line(x)), And(Not(line(o)), Not(open()))))
    Goal(oplayer, Score(0), line(x))

    todo
  }

}