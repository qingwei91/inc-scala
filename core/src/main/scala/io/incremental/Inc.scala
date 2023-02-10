package io.incremental

import cats.{FlatMap, Monad}
import cats.syntax.all.*

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.hashing.*

sealed trait Inc[+A] {
  def map[B](f: A => B): Inc[B]                      = MapInc(this, f)
  def flatMap[B](f: A => Inc[B]): Inc[B]             = FMapInc(this, f)
  def map2[B, C](ib: Inc[B], f: (A, B) => C): Inc[C] = Map2Inc(this, ib, f)
}
class InputInc[A](val init: A) extends Inc[A]

extension [A](i: InputInc[A]) {
  def toInc: Inc[A] = i
}

case class MapInc[A, B](i: Inc[A], f: A => B)                       extends Inc[B]
case class Map2Inc[A, B, C](i1: Inc[A], i2: Inc[B], f: (A, B) => C) extends Inc[C]
case class FMapInc[A, B](i: Inc[A], f: A => Inc[B])                 extends Inc[B]

object Inc {
  def init[A](a: A): InputInc[A] = new InputInc[A](a)
}

/** How to avoid dynamic compute on Height? We need to store height and only update when needed
  *   1. Every node store height as variable, not def
  *   1. When looping through the tree, we know for sure the height of input node, its either 0 or some n+1 where n is height of a flatmap
  *      node that generates the input.
  *   1. Then when registering a node during static creation, we
  */

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
      inode.changed = true
      inode.data = value
    }
  }

  def observe[A](inc: Inc[A]): Observed[A] = {
    val internal = icToNode(inc)
    new Observed[A] {
      override def read: A = internal.data.asInstanceOf
    }
  }

  def addInc[A](ic: Inc[A], forceRecompute: Boolean = false, startingHeight: Int = 0): InternalNode = {
    if (icToNode.contains(ic)) {
      return icToNode(ic)
    }

    val internalNode: InternalNode = ic match {
      case ic: InputInc[A] => INode(ic.init, true, height = startingHeight)
      case MapInc(i, f) =>
        val dep  = addInc(i, forceRecompute, startingHeight)
        val next = MNode(null, eval = f.asInstanceOf[Any => Any], dep)
        dep.pushOutNodes.addOne(next)
        next
      case Map2Inc(i1, i2, f) =>
        val node1 = addInc(i1, forceRecompute, startingHeight)
        val node2 = addInc(i2, forceRecompute, startingHeight)
        val next  = M2Node(null, f.asInstanceOf, (node1, node2))
        node1.pushOutNodes.addOne(next)
        node2.pushOutNodes.addOne(next)
        next
      case FMapInc(i, f) =>
        // a flatmap node is really a collection of nodes, with a start and an end, then the middle is created dynamically
        // we use the RedirectNode to represent
        val depNode   = addInc(i)
        val innerNode = RedirectNode(null, depNode, depNode.height + 2)
        val fmNode    = FMNode(f.asInstanceOf, depNode, innerNode)
        increaseToHeight(depNode.height + 1)
        nodesInHeight(depNode.height + 1).addOne(fmNode)
        innerNode
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
    if (desiredHeight > innerNode.height) {
      nodesInHeight(innerNode.height).filterInPlace(_ ne innerNode)
      nodesInHeight(desiredHeight).append(innerNode)
      innerNode.height = desiredHeight
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
          // this work because we know ith node is removed, so the next node is still i, thus we retract
          i -= 1
          increaseToHeight(correctHeight)
          nodesInHeight(correctHeight).append(node)
        } else {
          node match {
            case node: INode =>
              if (node.changed) {
                changedNodes.put(node, true)
                node.changed = false
              }
            case node: MNode =>
              if (changedNodes.containsKey(node.dep)) {
                val updated = node.eval(node.dep.data)
                if (updated != node.data) {
                  node.data = updated
                  changedNodes.put(node, true)
                }
              }
            case node @ FMNode(eval, dep, forwardNode) =>
              if (changedNodes.containsKey(dep)) {
                val newInnerTree = eval(dep.data)
                /* todo: this is buggy, a node added might be an existing node, in which case no change required,
                    it might also be a newly added node, depending on an existing, then the newly added node,
                    for example a map depending on an existing Input need to have height of n + 1 where n is current height of FMap.*/
                val outputNode = addInc(newInnerTree, true, node.height + 1)

                forwardNode.dep = outputNode
                val innerNodeNewHeight = outputNode.height + 1
                relocateInnerNode(forwardNode, innerNodeNewHeight)
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
