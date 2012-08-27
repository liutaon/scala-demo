package interview
/*
第15 题：
题目：输入一颗二元查找树，将该树转换为它的镜像，
即在转换后的二元查找树中，左子树的结点都大于右子树的结点。
用递归和循环两种方法完成树的镜像转换。
例如输入：
8
/ \
6 10
/\ /\
5 7 9 11
输出：
  8
 /  \
10   6
/\   /\
11 9 7 5
定义二元查找树的结点为：
struct BSTreeNode // a node in the binary search tree (BST)
{
int m_nValue; // value of node
BSTreeNode *m_pLeft; // left child of node
BSTreeNode *m_pRight; // right child of node
};
*/
object I015 extends Demo {
    implicit def wrapper(node: Node) = new MyNode(node)
    case class MyNode(node: Node) {
        def mirror: Node = {
            val t = node.left
            node.left = node.right
            node.right = t
            if (node.left != null) node.left.mirror
            if (node.right != null) node.right.mirror
            node
        }

        def mirror2: Node = {
            import scala.collection.mutable
            val stack = mutable.Stack[Node]()
            stack.push(node)
            while (!stack.isEmpty) {
                val node = stack.pop
                val t = node.left
                node.left = node.right
                node.right = t
                if (node.left != null) stack.push(node.right)
                if (node.right != null) stack.push(node.left)
            }
            node
        }
    }

    def test() = {
        val root = Node.from(List(8,6,10,5,7,9,11))
        println(root)
        println(root.mirror)
        println(root.mirror2)
    }
}