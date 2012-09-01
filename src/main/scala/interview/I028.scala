package interview
/*
28.整数的二进制表示中1 的个数
题目：输入一个整数，求该整数的二进制表达中有多少个1。
例如输入10，由于其二进制表示为1010，有两个1，因此输出2。
分析：
这是一道很基本的考查位运算的面试题。
包括微软在内的很多公司都曾采用过这道题。
*/
object I028 extends Demo {

    // ha ha ha
    def find1(value: Int) = {
        value.toBinaryString.count(_ == '1')
    }

    def find2(value: Int) = {
        var c = 0
        var n = value
        while (n != 0) {
            n = n & (n - 1)
            c += 1
        }
        c
    }

    def find3(value: Int) = {
        var c = 0
        for (i <- 0 to 31; t = 1 << i; if t <= value) {
            if ((t & value) == t) c += 1
        }
        c
    }

    def test() = {
        List(1234567, Int.MaxValue, Int.MinValue).foreach { t =>
            println(find1(t))
            println(find2(t))
            println(find3(t))
        }
    }
}