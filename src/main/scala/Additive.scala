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

  case class Point3d(x: Int, y: Int, z: Int)

  object Point3d {

    implicit object Point3dAdditive extends Additive[Point3d] {
      def plus(a: Point3d, b: Point3d): Point3d = {
        Point3d(a.x + b.x, a.y + b.y, a.z + b.z)
      }
      def zero: Point3d = Point3d(0, 0, 0)
    }
  }

  def sum[A](lst: List[A])(implicit m: Additive[A]): A = lst.foldLeft(m.zero)((x, y) => m.plus(x, y))
}
