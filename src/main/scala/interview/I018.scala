package interview
/*
第18 题：
题目：n 个数字（0,1,…,n-1）形成一个圆圈，从数字0 开始，
每次从这个圆圈中删除第m 个数字（第一个为当前数字本身，第二个为当前数字的下一个数
字）。
当一个数字删除后，从被删除数字的下一个继续删除第m 个数字。
求出在这个圆圈中剩下的最后一个数字。
*/
object I018 extends Demo {
    case class Find(input: List[Int], m: Int) {
        val list = input.toBuffer

        def find(start: Int = 0): Int = {
            if (list.size == 1) return list(0)
            val next = (start + m - 1) % list.size
            list.remove(next)
            find(if (next == list.size) 0 else next)
        }

        def find2(): Int = {
            var ret = 0
            (2 to input.size).foreach(i => ret = (ret + m) % i)
            ret
        }
    }

    def test() = {
        val list = (0 to 100).toList
        println(Find(list, 88).find())
        println(Find(list, 88).find2())        
    }
}