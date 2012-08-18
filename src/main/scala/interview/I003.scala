package interview
/*
3.求子数组的最大和（数组）
题目：
输入一个整形数组，数组里有正数也有负数。
数组中连续的一个或多个整数组成一个子数组，每个子数组都有一个和。
求所有子数组的和的最大值。要求时间复杂度为O(n)。
例如输入的数组为1, -2, 3, 10, -4, 7, 2, -5，和最大的子数组为3, 10, -4, 7, 2，
因此输出为该子数组的和18。
*/
/*
原理: sum 记录当前计算的和，total 记录历史最大值，当 sum 变成负数时(因为此时已没有意义)，就重新计算。
*/
object I003 extends App {
	def maxsum(input: List[Int]): Int = {
		var total = 0
		var sum = 0
		input.foreach { i =>
			if (sum <= 0) {
				sum = i
			} else {
				sum += i
			}
			if (total < sum) {
				total = sum
			}
		}
		total
	}

	val input = List(1,-2,3,10,-4,7,2,-5)
	println(maxsum(input))
}