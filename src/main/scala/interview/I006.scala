package interview
/*
第6题
------------------------------------
腾讯面试题：  
给你10分钟时间，根据上排给出十个数，在其下排填出对应的十个数  
要求下排每个数都是先前上排那十个数在下排出现的次数。  
上排的十个数如下：  
【0，1，2，3，4，5，6，7，8，9】
举一个例子:
数值: 0,1,2,3,4,5,6,7,8,9  
分配: 6,2,1,0,0,0,1,0,0,0  
*/
// 个人认为此题有歧义，若输入为0,1,2，则无法确定输出什么。
object I006 extends App {
	case class FindCounter(input: Seq[Int]){
		private val counter = Array.fill(input.size)(-1)
		private var ok = false

		def find: Seq[Int] = {
			var i = 0
			while(!ok && i <= input.size) {
				i += 1
				setNextBottom
			}
			return counter
		}

		def setNextBottom() {
			var reB = true
			(0 until input.size).foreach { i =>
				val fre = getFrequecy(i)
				if (counter(i) != fre) {
					counter(i) = fre
					reB = false
				}
			}
			ok = reB
		}

		def getFrequecy(num: Int) = counter.count(_ == num)
	}

	println(FindCounter(List(0,1,2,3,4,5,6,7,8,9)).find)
	println(FindCounter(List(0,1,2,3)).find)
	println(FindCounter(List(0,1,2)).find)
	println(FindCounter(List(0,1,2,3,4,5,6)).find)
}