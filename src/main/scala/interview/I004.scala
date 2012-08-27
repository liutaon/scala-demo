package interview
/*
4.在二元树中找出和为某一值的所有路径（树）
题目：输入一个整数和一棵二元树。
从树的根结点开始往下访问一直到叶结点所经过的所有结点形成一条路径。
打印出和与输入整数相等的所有路径。
例如 输入整数22和如下二元树
  10  
  / \   
 5  12   
 / \   
4   7
则打印出两条路径：10, 12和10, 5, 7。
*/

object I004 extends Demo {
    import scala.collection.mutable.ArrayStack
    implicit def wrapper(node: Node) = new MyNode(node)
    case class MyNode(node: Node) {
        def findSum(sum: Int, stack: ArrayStack[Int]): Unit = {
            stack.push(node.value)
            try {
                if (node.value == sum && node.isleaf) {
                    println(stack.toList.reverse.mkString(","))
                } else {
                    if (node.left != null) node.left.findSum(sum - node.value, stack)
                    if (node.right != null) node.right.findSum(sum - node.value, stack)
                }
            } finally {
                stack.pop
            }
        }
    }

    def test() = {
        val node = Node.from(List(10,5,12,4,7))
        node.findSum(22, ArrayStack[Int]())
    }
}