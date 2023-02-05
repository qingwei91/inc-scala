package io.incremental

import cats.{FlatMap, Monad}
import cats.syntax.all.*

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.hashing.*

sealed trait Inc[+A]
class InputInc[A](val init: A) extends Inc[A]

extension [A](i: InputInc[A]) {
  def toInc: Inc[A] = i
}

case class MapInc[A, B](i: Inc[A], f: A => B)                       extends Inc[B]
case class Map2Inc[A, B, C](i1: Inc[A], i2: Inc[B], f: (A, B) => C) extends Inc[C]
case class FMapInc[A, B](i: Inc[A], f: A => Inc[B])                 extends Inc[B]

object Inc {
  def init[A](a: A): InputInc[A] = new InputInc[A](a)

  implicit val fm: FlatMap[Inc] = new FlatMap[Inc] {
    override def map[A, B](fa: Inc[A])(f: A => B): Inc[B] = {
      MapInc(fa, f)
    }

    override def flatMap[A, B](fa: Inc[A])(f: A => Inc[B]): Inc[B] = FMapInc(fa, f)

    override def map2[A, B, Z](fa: Inc[A], fb: Inc[B])(f: (A, B) => Z): Inc[Z] = Map2Inc(fa, fb, f)

    override def tailRecM[A, B](a: A)(f: A => Inc[Either[A, B]]): Inc[B] = {
      ???
    }
  }
}

class StateGraph(
    val nodesInHeight: mutable.ArrayBuffer[mutable.ArrayBuffer[InternalNode]],
    val icToNode: mutable.HashMap[Inc[_], InternalNode]
) {

  trait Observed[A] {
    def read: A
  }

  def update[A](input: InputInc[A], value: A): Unit = {
    val inode = icToNode(input).asInstanceOf[INode]
    if (inode.data != value) {
      inode.recomputed = true
      inode.data = value
    }
  }

  def observe[A](inc: Inc[A]): Observed[A] = {
    val internal = icToNode(inc)
    new Observed[A] {
      override def read: A = internal.data.asInstanceOf
    }
  }

  def addInc[A](ic: Inc[A], startingHeight: Int = 0): InternalNode = {
    if (icToNode.contains(ic)) {
      return icToNode(ic)
    }

    val internalNode: InternalNode = ic match {
      case ic: InputInc[A] => INode(ic.init)
      case MapInc(i, f) =>
        val dep = addInc(i, startingHeight)
        MNode(null, eval = f.asInstanceOf, dep)
      case Map2Inc(i1, i2, f) =>
        val node1 = addInc(i1, startingHeight)

        val node2 = addInc(i2, startingHeight)
        M2Node(null, f.asInstanceOf, (node1, node2))
      case FMapInc(i, f) =>
        // a flatmap node is really a collection of nodes, with a start and an end, then the middle is created dynamically
        val depNode = addInc(i)
        val inner   = RedirectNode(null, depNode, depNode.height + 2)
        increaseToHeight(depNode.height + 2)
        nodesInHeight(depNode.height + 2).addOne(inner)
        FMNode(f.asInstanceOf, depNode, inner)
    }

    icToNode.addOne(ic -> internalNode)
    increaseToHeight(internalNode.height + 1)
    nodesInHeight(internalNode.height).append(internalNode)
    internalNode
  }

  def increaseToHeight(targetHeight: Int): Unit = {
    if (nodesInHeight.size <= targetHeight) {
      val neededMore = targetHeight - nodesInHeight.size + 1
      nodesInHeight.appendAll(mutable.ArrayBuffer.fill(neededMore)(mutable.ArrayBuffer.empty[InternalNode]))
    }
  }

  private def relocateInnerNode(innerNode: RedirectNode, desiredHeight: Int) = {
    if (desiredHeight < innerNode.currentHeight) {
      nodesInHeight(innerNode.currentHeight).filterInPlace(_ ne innerNode)
      nodesInHeight(desiredHeight).append(innerNode)
      innerNode.currentHeight = desiredHeight
    }
  }

  def compute(): Unit = {
    val changedNodes  = java.util.IdentityHashMap[InternalNode, Boolean]
    var currentHeight = 0
    while (currentHeight < nodesInHeight.size) {
      val nodes = nodesInHeight(currentHeight)

      var i = 0
      while (i < nodes.size) {
        val node          = nodes(i)
        val correctHeight = math.max(currentHeight, node.height)
        if (currentHeight != correctHeight) {
          nodes.filterInPlace(_ != node)
          increaseToHeight(correctHeight)
          nodesInHeight(correctHeight).append(node)
        } else {
          node match {
            case node: INode =>
              if (node.recomputed) {
                changedNodes.put(node, true)
                node.recomputed = false
              }
            case node: MNode =>
              if (changedNodes.containsKey(node.dep)) {
                val updated = node.eval(node.dep.data)
                if (updated != node.data) {
                  node.data = updated
                  changedNodes.put(node, true)
                }
              }
            case node @ FMNode(eval, dep, innerNode) =>
              if (changedNodes.containsKey(dep)) {
                val newInnerTree = eval(dep.data)
                val outputNode   = addInc(newInnerTree, node.height + 1)

                innerNode.dep = outputNode
                val innerNodeNewHeight = outputNode.height + 1
                relocateInnerNode(innerNode, innerNodeNewHeight)
              }
            case node @ M2Node(data, eval, (dep1, dep2)) =>
              if (changedNodes.containsKey(dep1) || changedNodes.containsKey(dep2)) {
                val updated = eval(dep1.data, dep2.data)
                if (updated != data) {
                  node.data = updated
                  changedNodes.put(node, true)
                }
              }
            case node @ RedirectNode(data, dep, currentHeight) =>
              if (changedNodes.containsKey(dep)) {
                node.data = dep.data
                changedNodes.put(node, true)
              }
          }
        }
        i += 1
      }
      currentHeight += 1
    }

  }
  def gc(): Unit = ???
}

object StateGraph {
  def init: StateGraph = new StateGraph(
    mutable.ArrayBuffer.fill(1)(mutable.ArrayBuffer.empty[InternalNode]),
    mutable.HashMap.empty
  )
}
