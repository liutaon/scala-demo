package interview
/*
第11题
------------------------------------
求二叉树中节点的最大距离...

  如果我们把二叉树看成一个图，
  父子节点之间的连线看成是双向的，
  我们姑且定义"距离"为两节点之间边的个数。
  写一个程序，
  求一棵二叉树中相距最远的两个节点之间的距离。
*/
object I011 extends App {
	case class Node(value: Int, var left: Node, var right: Node) {
		def add(t: Int): Node = t.compare(value) match {
			case i if i < 0 && left != null => left.add(t)
			case i if i < 0 && left == null => left = Node(t);this
			case _ if right != null => right.add(t)
			case _ if right == null => right = Node(t);this
		}

		def add(tt: List[Int]): Node = { tt.foreach(add(_)); this }
	}

	object Node {
		def apply(i: Int) = new Node(i, null, null)

		def from(ii: List[Int]) = ii match {
			case Nil => null
			case head :: xs => new Node(head, null, null).add(xs)
		}
	}

	def height(node: Node): Int = {
		if (node == null) 0
		else {
			var l = 0
			var r = 0
			if (node.left != null) {
				l = 1 + height(node.left)
			}
			if (node.right != null) {
				r = 1 + height(node.right)
			}
			math.max(r, l)
		}
	}

	val root = Node.from(List(5,9,3,6,7,2,1,8,10,11,67,43,35))
	println(height(root.left) + height(root.right) + 2)

}