object Additives {

  trait Additive[A]{
    def plus(a:A,b:A) :A
    def zero : A
  }

  implicit object StringAdditive extends Additive[String]{
    def plus(a:String,b:String):String = a + b
    def zero:String = ""
  }

  implicit object IntAdditive extends Additive[Int]{
    def plus(a:Int,b:Int):Int = a + b
    def zero:Int = 0
  }

  implicit object DoubleAdditive extends Additive[Double]{
    def plus(a:Double,b:Double):Double = a + b
    def zero:Double = 0.0
  }

  case class Point(x:Int,y:Int)
  case class DPoint(x:Double,y:Double)

  object Point {
    implicit object PointAdditive extends Additive[Point] {
      def plus(a: Point, b: Point): Point = Point(a.x+b.x,a.y+b.y)
      def zero:Point = Point(0,0)
    }

  }
  object DPoint {
    implicit object PointAdditive extends Additive[DPoint] {
      def plus(a: DPoint, b: DPoint): DPoint = DPoint(a.x+b.x,a.y+b.y)
      def zero:DPoint = DPoint(0.0,0.0)
    }

  }


  def sum[A](list:List[A])(implicit m:Additive[A]):A = list.foldLeft(m.zero)((a,b)=>m.plus(a,b))

}
