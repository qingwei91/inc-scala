package io.incremental

import cats.syntax.all._
import scala.collection.mutable

class IncTest extends munit.FunSuite {
  def inlineExp(a: Double, b: Double, c: Double): (Double, Double) = {
    val plus  = (-b + math.sqrt((b * b) - (4 * a * c))) / (2 * a)
    val minus = (-b - math.sqrt((b * b) - (4 * a * c))) / (2 * a)
    (plus, minus)
  }

  test("InputInc identity hash") {
    val mapp = mutable.HashMap.empty[Inc[_], Int]
    val a = Inc.init(2.0)
    mapp.addOne(a, 1)

    assertEquals(mapp(a), 1)
  }

  test("quadratic factor") {
    val graph: StateGraph = StateGraph.init

    val a = Inc.init(1.0)
    val b = Inc.init(0.1)
    val c = Inc.init(-2.0)

    val fourAc: Inc[Double] = a.toInc.map2(c)((a, c) => 4 * a * c)
    val sqrtB: Inc[Double]  = b.toInc.map2(fourAc)((b, fac) => math.sqrt((b * b) - fac))
    val plusSolution        = b.toInc.map2(sqrtB)((b, sqC) => -b + sqC).map2(a)((top, a) => top / (2 * a))
    val minusSolution       = b.toInc.map2(sqrtB)((b, sqC) => -b - sqC).map2(a)((top, a) => top / (2 * a))

    graph.addInc(plusSolution)
    graph.addInc(minusSolution)

    val plusHandle  = graph.observe(plusSolution)
    val minusHandle = graph.observe(minusSolution)

    graph.update(a, 3)
    graph.update(b, 2)
    graph.update(c, -1)
    graph.compute()

    val (expectedPlus, expectedMinus) = inlineExp(3, 2, -1)

    assertEquals(plusHandle.read, expectedPlus)
    assertEquals(minusHandle.read, expectedMinus)
  }
}
