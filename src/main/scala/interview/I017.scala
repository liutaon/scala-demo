package interview
/*
第17 题：
题目：在一个字符串中找到第一个只出现一次的字符。如输入 abaccdeff, 则输出b。
分析：这道题是2006 年google 的一道笔试题。
*/
object I017 extends Demo {
    def find(input: String): Option[Char] = {
        val map = Array.fill(26)(0)
        input.foreach { c => c.toLower match {
            case i if i <= 'z' && i >= 'a' => {
                map(i - 'a') = map(i - 'a') + 1
            }
            case _ =>
        }}
        map.indexWhere(_ == 1) match {
            case -1 => None
            case i => Some((i + 'a').toChar)
        }
    }

    def test() = {
        println(find("abaccdeff"))
    }
}