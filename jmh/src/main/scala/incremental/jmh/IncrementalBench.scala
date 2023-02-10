package incremental.jmh

import cats.syntax.all.*
import io.incremental.*
import org.openjdk.jmh.annotations.*

import scala.collection.mutable

@State(Scope.Benchmark)
@Fork(
  jvmArgsAppend = Array(
    "-agentpath:/Applications/YourKit-Java-Profiler-2022.9.app/Contents/Resources/bin/mac/libyjpagent.dylib=disablestacktelemetry,exceptions=disable,delay=10000,listen=all"
  )
)
class IncrementalBench {

  def inlineExp(a: Double, b: Double, c: Double): (Double, Double) = {
    val plus  = (-b + math.sqrt((b * b) - (4 * a * c))) / (2 * a)
    val minus = (-b - math.sqrt((b * b) - (4 * a * c))) / (2 * a)
    (plus, minus)
  }

  @Param(Array("-10000"))
  var a: Double = 0
  @Param(Array("-10000"))
  var b: Double = 0

  @Param(Array("-10000"))
  var c: Double = 0

//  @Benchmark
//  def baseline() = {
//    inlineExp(a, b, c)
//  }

  @Benchmark
  def incrementalPureOverhead(state: MyState) = {
    state.graph.update(state.a, 0)
    state.graph.update(state.b, 0)
    state.graph.update(state.c, 0)

    state.graph.update(state.a, a)
    state.graph.update(state.b, b)
    state.graph.update(state.c, c)
    state.graph.compute()
  }
}

@State(Scope.Benchmark)
class MyState() {
  var a: InputInc[Double] = _
  var b: InputInc[Double] = _
  var c: InputInc[Double] = _
  var graph: StateGraph   = _

  @Setup(Level.Iteration)
  def setupGraph(): Unit = {
    a = Inc.init(1.0d)
    b = Inc.init(0.1d)
    c = Inc.init(-2.0d)
    graph = StateGraph.init
    val fourAc: Inc[Double] = a.toInc.map2(c, (a, c) => 4 * a * c)
    val sqrtB: Inc[Double]  = b.toInc.map2(fourAc, (b, fac) => math.sqrt((b * b) - fac))
    val plusSolution        = b.toInc.map2(sqrtB, (b, sqC) => -b + sqC).map2(a, (top, a) => top / (2 * a))
    val minusSolution       = b.toInc.map2(sqrtB, (b, sqC) => -b - sqC).map2(a, (top, a) => top / (2 * a))

    graph.addInc(plusSolution)
    graph.addInc(minusSolution)
  }

}
