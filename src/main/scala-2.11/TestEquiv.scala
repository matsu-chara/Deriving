import shapeless._
import HList._

// case classをHListにimplicit conversionしてequal判定
object TestEquiv {

  case class Example[A, B](a: A, b: Seq[B], c: Int)

  trait Equiv[T] {
    def equiv(a: T, b: T): Boolean
  }

  // Equivインスタンスの定義
  implicit val intInstance = new Equiv[Int] {
    def equiv(a: Int, b: Int) = a == b
  }

  implicit val stringInstance = new Equiv[String] {
    def equiv(a: String, b: String) = a == b
  }

  implicit def seqInstance[A](implicit A: Equiv[A]) = new Equiv[Seq[A]] {
    def equiv(a: Seq[A], b: Seq[A]) =
      a.zip(b).forall(e => A.equiv(e._1, e._2))
  }

  // HListとの変換
  implicit def to[A, B](e: Example[A, B]): A :: Seq[B] :: Int :: HNil =
    e.a :: e.b :: e.c :: HNil

  implicit def from[A, B](hl: A :: Seq[B] :: Int :: HNil): Example[A, B] = hl match {
    case a :: b :: c :: HNil => Example(a, b, c)
  }

  // HListのEqインスタンス
  implicit def equivHNil: Equiv[HNil] = new Equiv[HNil] {
    def equiv(a: HNil, b: HNil) = true
  }

  implicit def equivHCons[H, T <: HList](implicit equivH: Equiv[H], equivT: Equiv[T]): Equiv[H :: T] =
    new Equiv[H :: T] {
      def equiv(a: H :: T, b: H :: T) =
        equivH.equiv(a.head, b.head) && equivT.equiv(a.tail, b.tail)
    }

  // HListへ変換可能なモノのEquivインスタンスを定義
  implicit def equivHListable[T, HL <: HList](implicit toHList: T => HL, equivHL: Equiv[HL]): Equiv[T] =
    new Equiv[T] {
      def equiv(a: T, b: T) =
        equivHL.equiv(toHList(a), toHList(b))
    }

  // ===などのメソッドを定義
  implicit class equivEq[A](a1: A) {
    def ===(a2: A)(implicit e: Equiv[A]) = e.equiv(a1, a2)

    def !==(a2: A)(implicit e: Equiv[A]) = !e.equiv(a1, a2)
  }

  def main(args: Array[String]) {
    val a = Example("a", Seq(1, 2, 3), 19)
    val b = Example("a", Seq(1, 2, 3), 19)
    println(a === b)

    assert(Example("b", Seq(1, 2, 3), 1) !== Example("a", Seq(1, 2, 3), 1))
  }
}