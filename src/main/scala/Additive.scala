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

  case class MyLang(status: String)

  object MyLang {
    // scala> sum(List(MyLang("Scala"), MyLang("Elixir")))
    // res1: Additives.MyLang = MyLang(Ruby and Scala and Elixir)
    implicit object MyLifeAdditive extends Additive[MyLang] {
      def plus(a: MyLang, b: MyLang): MyLang =
        MyLang(a.status + " and " + b.status)

      def zero: MyLang = MyLang("Ruby")
    }
  }

  def sum[A](lst: List[A])(implicit m: Additive[A]): A =
    lst.foldLeft(m.zero)((x, y) => m.plus(x, y))
}
