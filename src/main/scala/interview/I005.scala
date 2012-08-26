package interview
/*
5.查找最小的k个元素（数组）
题目：输入n个整数，输出其中最小的k个。
例如输入1，2，3，4，5，6，7和8这8个数字，则最小的4个数字为1，2，3和4。
*/
class HeapTree (size: Int)(f: (Int, Int) => Boolean) {
	private val items = new Array[Int](size)
	private var s = 0

	def add(v: Int) {
		if (s == size) {
			if (f(v, items(0))) return
			remove
			add(v)
		} else {
			s += 1
			items(s - 1) = v
			siftup(s - 1)
		}
	}

	def remove() = {
		if (s == 0) throw new Exception("empty")
		val top = items(0)
		items(0) = items(s - 1)
		s -= 1
		if (s > 0) {
			siftdown(0)
		}
		top
	}

	def count = s

	private def siftup(idx: Int) {
		val p = parent(idx)
		if (f(items(idx), items(p))) {
			swap(idx, p)
			siftup(p)
		}
	}

	private def siftdown(idx: Int) {
		val l = left(idx)
		val r = right(idx)
		var largest = if (l < s && f(items(l), items(idx))) l else idx
		largest = if (r < s && f(items(r), items(largest))) r else largest
		if (largest != idx) {
			swap(idx, largest)
			siftdown(largest)
		}
	}

	private def swap(i: Int, j: Int): Unit = {
		val v = items(i);
		items(i) = items(j);
		items(j) = v
	}

	private def parent(idx: Int):Int = idx / 2
	private def left(idx: Int):Int = 2 * idx + 1
	private def right(idx: Int):Int = 2 * idx + 2
}

object I005 extends Demo {
	def test(input: Seq[Int]) {
		val heap = new HeapTree(4)((a, b) => a > b)
		input.foreach(heap.add(_))
		(1 to 4).foreach(_ => println(heap.remove))
	}
	
	def test() = {
		test(List(1,2,3,4,5,6,7,8))
		test(List(9,5,3,13,6,1,7,10))
	}
}