object P01 {

	def lastBuiltin[A](ls: List[A]) : A = ls.last)

	def last[A](xs: List[A]) : A = {
		xs match {
			case h :: Nil => h
			case _ :: tail => last(tail)
			case _ => throw new NoSuchElementException
		}
	}

}
