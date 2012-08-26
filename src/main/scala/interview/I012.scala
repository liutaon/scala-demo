package interview
/*
第12 题
题目：求1+2+…+n，
要求不能使用乘除法、for、while、if、else、switch、case 等关键字以及条件判断语句
（A?B:C）。
*/
object I012 extends Demo {
	case class Sum1(n: Int){
		private var t = n
		def sum = {
			c(0);c(1);c(2);c(3);c(4)
			c(5);c(6);c(7);c(8);c(9)
			c(10);c(11);c(12);c(13);c(14)
			c(15);c(16);c(17);c(18);c(19)
			c(20);c(21);c(22);c(23);c(24)
			c(25);c(26);c(27);c(28);c(29)
			c(30);c(31)
			t = t >> 1
			t
		}

		private def c(i: Int) = ((n & (1 << i)) != 0) && {t += (n << i);t != 0}
	}

	case class Sum2(n: Int) {
		def sum(): Int = sum(n)

		def sum(t: Int): Int = {
			var v = 0
			t > 0 && {v = t + sum(t - 1);true}
			v
		}
	}

	def test() = {
		println(Sum1(100).sum)
		println(Sum2(100).sum)
		println((1 to 100).sum)
	}
}