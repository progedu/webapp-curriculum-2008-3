object Additives {

  trait Additive[A] {
    def plus(a: A, b: A): A

    def zero: A
  }

  implicit object StringAdditive extends Additive[String] {
    def plus(a: String, b: String): String = a + b

    def zero: String = ""
  }

  implicit object IntAdditive extends Additive[Int] {
    def plus(a: Int, b: Int): Int = a + b

    def zero: Int = 0
  }

  case class Matrix(x11: Int, x12: Int, x21: Int, x22: Int)

  object Matrix {
    implicit object MatrixAdditive extends Additive[Matrix] {
      def plus(a: Matrix, b: Matrix): Matrix = {
        Matrix(a.x11 + b.x11, a.x12 + b.x12, a.x21 + b.x21, a.x22 + b.x22)
      }
      def zero: Matrix = Matrix(0, 0, 0, 0)
    }
  }
  
  def sum[A](lst: List[A])(implicit m: Additive[A]): A = lst.foldLeft(m.zero)((x, y) => m.plus(x, y))
}
