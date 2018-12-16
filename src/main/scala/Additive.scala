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

  case class MyVector(x: Int, y: Int, z: Int)

  object MyVector {

    implicit object VectorAdditive extends Additive[MyVector] {
      def plus(a: MyVector, b: MyVector): MyVector = {
        if (a == zero) {
          b
        } else if (b == zero) {
          a
        } else {
          MyVector(a.x + b.x, a.y + b.y, a.z + b.z)
        }
      }

      def zero: MyVector = MyVector(0, 0, 0)
    }

  }

  def sum[A](lst: List[A])(implicit m: Additive[A]): A = lst.foldLeft(m.zero)((x, y) => m.plus(x, y))
}
