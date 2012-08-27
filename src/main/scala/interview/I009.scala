package interview
/*
第9题
-----------------------------------
判断整数序列是不是二元查找树的后序遍历结果
题目：输入一个整数数组，判断该数组是不是某二元查找树的后序遍历的结果。
如果是返回true，否则返回false。
例如输入5、7、6、9、11、10、8，由于这一整数序列是如下树的后序遍历结果：
    8
   / \
  6   10
 / \  / \
 5 7  9 11
因此返回true。
如果输入7、4、6、5，没有哪棵树的后序遍历的结果是这个序列，因此返回false。
*/
object I009 extends Demo {
    def check(input: Seq[Int]): Boolean = {
        check(input, _ < Int.MaxValue)
    }

    def check(input: Seq[Int], f: Int => Boolean): Boolean = {
        if (input.size <= 1) {
            return true
        }
        if (input.exists(!f(_))) {
            return false
        }
        val last = input.last
        val left = input.lastIndexWhere(_ < last)
        val right = input.lastIndexWhere(_ > last)
        if (left >= right) {
            return false
        }
        check(input.view(0, left + 1), _ < last) && check(input.view(left + 1, right + 1), _ > last)
    }

    def test() = {
        println(check(List(5,7,6,9,11,10,8)))
        println(check(List(7,4,6,5)))
        println(check(List(3,2,1)))
    }
}