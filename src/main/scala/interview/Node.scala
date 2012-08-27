package interview
/*
Binary Tree
*/
case class Node(value: Int, var left: Node, var right: Node) {
    def add(t: Int): Node = t.compare(value) match {
        case i if i < 0 && left != null => left.add(t)
        case i if i < 0 && left == null => left = Node(t);this
        case _ if right != null => right.add(t)
        case _ if right == null => right = Node(t);this
    }

    def add(tt: List[Int]): Node = { tt.foreach(add(_)); this }
    def isleaf = left == null && right == null
}

object Node {
    def apply(i: Int) = new Node(i, null, null)

    def from(ii: List[Int]): Node = ii match {
        case Nil => null
        case head :: xs => new Node(head, null, null).add(xs)
    }

    def from(ints: Int*): Node = from(ints.toList)
}