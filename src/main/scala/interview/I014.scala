package interview
/*
第14 题：
题目：输入一个已经按升序排序过的数组和一个数字，
在数组中查找两个数，使得它们的和正好是输入的那个数字。
要求时间复杂度是O(n)。如果有多对数字的和等于输入的数字，输出任意一对即可。
例如输入数组1、2、4、7、11、15 和数字15。由于4+11=15，因此输出4 和11。
*/
object I014 extends Demo {

	def find(input: List[Int], sum: Int):Option[(Int, Int)] = {
		var head = 0
		var last = input.size - 1
		var ret: Option[(Int, Int)] = None
		while (!ret.isDefined && last >= 0 && head < input.size) {
			val total = input(head) + input(last)
			if (total > sum) last -= 1
			else if (total < sum) head += 1
			else ret = Some(input(head), input(last))
		}
		ret
	}

	def test() = {
		println(find(List(1,2,4,7,11,15), 15))
	}
}