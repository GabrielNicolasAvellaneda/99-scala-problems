
object P02 {

	def penultimateBuiltin[A](xs:List[A]) : A = {
		if (xs.isEmpty) throw new NoSuchElementException
		xs.init.last
	}

	def penultimate[A](xs:List[A]) : A = xs match {
		case h :: List(l) => h
		case _ :: tail => penultimate(tail) 
		case _ => throw new NoSuchElementException
	}

	def lastNthBuiltin[A](n: Int, xs:List[A]) : A = {
		if (n <= 0) throw new IllegalArgumentException
		if (n > xs.length) throw new NoSuchElementException
		xs.takeRight(n).head
	}

	def lastNth2[A](n: Int, xs:List[A]) : A = xs match {
		case l if (l.length == n) => l.head
		case _ :: tail => lastNth2(n, tail)  
		case _ => throw new NoSuchElementException
	}	

	def lastNth[A](n: Int, xs:List[A]) : A = {
		def _lastNth[A](n: Int, resultList:List[A], xs:List[A]) : A = {
			xs match {
				case Nil if n > 0 => throw new NoSuchElementException
				case Nil => resultList.head
				case _ :: tail => _lastNth(n - 1, if (n > 0) resultList else resultList.tail, tail) 
			}
		}

		if (n <= 0) throw new IllegalArgumentException
		else _lastNth(n, xs, xs)
	}

}
