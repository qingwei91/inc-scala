package io.incremental

import scala.collection.mutable

sealed trait InternalNode {
  def data: Any
  def height: Int
  val pushOutNodes: mutable.ArrayBuffer[InternalNode] = new mutable.ArrayBuffer(10)
}
case class INode(var data: Any, var changed: Boolean = false, height: Int = 0) extends InternalNode
case class MNode(var data: Any, eval: Any => Any, dep: InternalNode) extends InternalNode {
  override def height: Int = dep.height + 1
}
case class FMNode(eval: Any => Inc[Any], dep: InternalNode, forwardNode: RedirectNode) extends InternalNode {
  override def height: Int = dep.height + 1

  override def data: Any = forwardNode.data
}
case class M2Node(var data: Any, eval: (Any, Any) => Any, deps: (InternalNode, InternalNode)) extends InternalNode {
  override def height: Int = math.max(deps._1.height, deps._2.height) + 1
}
// this is only used in FlatMapNode as an anchor, currentHeight is used to track its current position
case class RedirectNode(var data: Any, var dep: InternalNode, var height: Int) extends InternalNode
