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

    def test() = {
        println(find1(Int.MaxValue))
        println(find2(Int.MaxValue))
    }
}