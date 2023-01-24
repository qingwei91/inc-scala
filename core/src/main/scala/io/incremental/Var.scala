package io.incremental

import scala.collection.mutable
import scala.util.hashing._

sealed trait Inc[+A]
class InputInc[A](val init: A) extends Inc[A] {
  override def hashCode(): Int = System.identityHashCode(this)
}
case class MapInc[A, B](i: Inc[A], f: A => B)                      extends Inc[B]
case class Map2Inc[A, B, C](i1: Inc[A], i2: Inc[B], f: (A, B) => C) extends Inc[C]
case class FMapInc[A, B](i: Inc[A], f: A => Inc[B])                 extends Inc[B]

sealed trait InternalNode {
  def data: Any
  def height: Int
}
case class INode(var data: Any, recomputed: Boolean = false) extends InternalNode {
  val height: Int = 0
}
case class MNode(var data: Any, eval: Any => Any, dep: InternalNode) extends InternalNode {
  override def height: Int = dep.height + 1
}
case class FMNode(eval: Any => Inc[Any], dep: InternalNode, innerNode: RedirectNode) extends InternalNode {
  override def height: Int = dep.height + 1

  override def data: Any = innerNode.data
}
case class M2Node(var data: Any, eval: (Any, Any) => Any, deps: (InternalNode, InternalNode)) extends InternalNode {
  override def height: Int = math.max(deps._1.height, deps._2.height) + 1
}

// this is only used in FlatMapNode as an anchor, currentHeight is used to track its current position
case class RedirectNode(var data: Any, var dep: InternalNode, var currentHeight: Int) extends InternalNode {
  override def height: Int = dep.height + 1
}

class StateGraph(
                  val nodesInHeight: mutable.ArrayBuffer[mutable.ArrayBuffer[InternalNode]],
                  val icToNode: mutable.HashMap[Inc[_], InternalNode]
) {

  def addInc[A](ic: Inc[A], startingHeight: Int = 0): InternalNode = {
    if (icToNode.contains(ic)) {
      icToNode(ic)
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
    val changedNodes: mutable.Set[InternalNode] = mutable.Set.empty
    var currentHeight                   = 0
    while (currentHeight <= nodesInHeight.size) {
      val nodes = nodesInHeight(currentHeight)
      for (i <- nodes.indices) {
        val node          = nodes(i)
        val correctHeight = math.max(currentHeight, node.height)
        if (currentHeight != correctHeight) {
          nodes.filterInPlace(_ != node)
          increaseToHeight(correctHeight)
          nodesInHeight(correctHeight).append(node)
        } else {
          node match {
            case node: INode => if (node.recomputed) { changedNodes.add(node) }
            case node: MNode =>
              if (changedNodes.contains(node.dep)) {
                val updated = node.eval(node.dep.data)
                if (updated != node.data) {
                  node.data = updated
                  changedNodes.add(node)
                }
              }
            case node @ FMNode(eval, dep, innerNode) =>
              if (changedNodes.contains(dep)) {
                val newInnerTree = eval(dep.data)
                val outputNode   = addInc(newInnerTree, node.height + 1)

                innerNode.dep = outputNode
                val innerNodeNewHeight = outputNode.height + 1
                relocateInnerNode(innerNode, innerNodeNewHeight)
              }
            case node @ M2Node(data, eval, (dep1, dep2)) =>
              if (changedNodes.contains(dep1) || changedNodes.contains(dep2)) {
                val updated = eval(dep1.data, dep2.data)
                if (updated != data) {
                  node.data = updated
                  changedNodes.add(node)
                }
              }
            case node @ RedirectNode(data, dep, currentHeight) =>
              if (changedNodes.contains(dep)) {
                node.data = dep.data
                changedNodes.add(node)
              }
          }
        }
      }
      currentHeight += 1
    }

  }
  def gc(): Unit = ???
}

/** Create Node * Pick an ID, largestID + 1 * All method that creates node need to depend on graph * Statically decide rank and place them
  * on the right place
  *
  * Core data structure: * List of Map, each element is a map contains all nodes of same height, sorted from low to high * each Map has node
  * id as key, and node as value * We need 2 operations:
  *   1. Add node of height 2. Change nodes height (both operation can arguably be 1 op, we need to be able to quickly find node by node id)
  *      3. Remove node by id
  *
  * Recompute * Start from height 0, recompute all node, * if a node if flatMap node, if we perform node expansion, then we might add new
  * nodes, we should track new nodes added within a flatmap so that we can remove them later, ie. gc
  */
