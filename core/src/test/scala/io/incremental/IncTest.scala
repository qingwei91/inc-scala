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
    val a    = Inc.init(2.0)
    mapp.addOne(a, 1)

    assertEquals(mapp(a), 1)
  }

  test("quadratic factor") {
    val graph: StateGraph = StateGraph.init

    val a = Inc.init(1.0)
    val b = Inc.init(0.1)
    val c = Inc.init(-2.0)

    val fourAc: Inc[Double] = a.map2(c, (a, c) => 4 * a * c)
    val sqrtB: Inc[Double]  = b.map2(fourAc, (b, fac) => math.sqrt((b * b) - fac))
    val plusSolution        = b.map2(sqrtB, (b, sqC) => -b + sqC).map2(a, (top, a) => top / (2 * a))
    val minusSolution       = b.map2(sqrtB, (b, sqC) => -b - sqC).map2(a, (top, a) => top / (2 * a))

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

  test("Dynamic node") {
    val input = Inc.init(202)
    val expr: Inc[Double] = input.toInc
      .flatMap { a =>
        val o: Inc[Double] = if (a % 2 == 0) {
          Inc.init(10).toInc.map(k => a * k / 102)
        } else {
          Inc.init(2022).toInc.map(k => (a - k) / 102).map(q => q * 3.2)
        }
        o
      }
      .map[Double](r => r * r * 0.8)

    val graph = StateGraph.init
    graph.addInc(expr)
    val handle = graph.observe(expr)

    graph.update(input, 111)
    graph.compute()

    assertEquals(handle.read, 2654.2080000000005)

    graph.update(input, 222)
    graph.compute()

    assertEquals(handle.read, 352.8)
  }

  test("Use static node in dynamic node") {
    val inputA = Inc.init(10)
    val treeA  = inputA.map(a => a + 10)

    val inputB = Inc.init(1)
    val treeB = inputB
      .map(q => q / 3)
      .map(k => k * 3)
      .flatMap { s =>
        if (s > 10) {
          treeA.map(oo => oo * s)
        } else {
          Inc.init(666)
        }
      }
      .map2(treeA, (a, b) => a + b)

    val graph = StateGraph.init
    graph.addInc(treeB)
    graph.update(inputB, 200)
    graph.compute()
    assertEquals(graph.observe(treeB).read, 20100)
  }

  test("No unnecessary recompute") {
    ???
  }
}
