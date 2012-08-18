package api.bstree

/**
 * BSTree.scala 整型二叉查找树
 */

sealed abstract class Node {
	def add(t: Int): Node = this match {
		case End => BSTreeNode(t, End, End)
		case BSTreeNode(v, left, right) => t.compare(v) match {
			case i if i < 0 => BSTreeNode(v, left.add(t), right)
			case _ => BSTreeNode(v, left, right.add(t)) 
		}
	}

	def join(node: Node): Unit = {}
}

case object End extends Node {
	  override def toString = "."
}

case class BSTreeNode(value: Int, var left: Node, var right: Node) extends Node {
	override def join(node: Node): Unit = node match {
		case End => this
		case p @ BSTreeNode(v, l, r) => {
			right = p
			p.left = this
		}
	}
	
	override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

object BSTreeNode {
	def apply(t: Int): BSTreeNode = new BSTreeNode(t, End, End)

	def fromList(ls: List[Int]): Node = ls.reverse match {
		case Nil => End
		case head :: xs => fromList(xs.reverse).add(head)
	}
}
