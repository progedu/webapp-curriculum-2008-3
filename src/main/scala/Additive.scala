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
  
  case class Point(x: Int, y: Int,z: Int)
  
  object Point {
    
    implicit object Point extends Additive[Point]{
      implicit object PointAdditive extends Additive[Point] {
        def plus(a: Point, b: Point, c: Point): Point = Point(a.x + b.x + c.x, a.y + b.y + c.y, a.z + b.z + c.z)

        def zero: Point = Point(0, 0, 0)
        
      }
    }
    
  }

  def sum[A](lst: List[A])(implicit m: Additive[A]): A = lst.foldLeft(m.zero)((x, y, z) => m.plus(x, y, z))
}
