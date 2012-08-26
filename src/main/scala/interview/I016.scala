package interview
/*
第16 题：
题目（微软）：
输入一颗二元树，从上往下按层打印树的每个结点，同一层中按照从左往右的顺序打印。
例如输入
 8
/ \
6 10
/ \ / \
5 7 9 11
输出8 6 10 5 7 9 11。
*/
object I016 extends Demo {
	import scala.collection.mutable
	def loop(node: Node)(f: Node => Unit) = {
		val q = mutable.Queue[Node]()
		q.enqueue(node)
		while (!q.isEmpty) {
			var size = q.size
			while (size != 0) {
				val node = q.dequeue
				f(node)
				size -= 1
				if (node.left != null) q.enqueue(node.left)
				if (node.right != null) q.enqueue(node.right)
			}
		}
	}

	def test() = {
		val root = Node.from(8,6,5,7,10,9,11)
		loop(root)(n => print(n.value + " "))
	}
}