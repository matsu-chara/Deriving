package Deriving

// （だいたい）任意のcase classをマクロでtupleに変換
object DerivingTest extends App {

  // テスト用ケースクラス
  case class Bar()
  case class Foo(i: Int, s: String, d: Double)
  case class Baz(i: Int, s: String, d: Double, f: Foo)

  // isoインスタンス
  val barIso = implicitly[Iso[Bar, Unit]]
  val fooIso = implicitly[Iso[Foo, (Int, String, Double)]]
  val bazIso = implicitly[Iso[Baz, (Int, String, Double, Foo)]]

  // フィールド数0のクラス
  val bar         : Bar  = Bar()
  val tupleFromBar: Unit = barIso.to(bar)
  val barFromTuple: Bar  = barIso.from(tupleFromBar)
  println(s"$tupleFromBar, $barFromTuple")

  // 普通のクラス
  val foo          = Foo(23, "foo", 4.3)
  val tupleFromFoo = fooIso.to(foo)
  val fooFromTuple = fooIso.from(tupleFromFoo)
  println(s"$foo, $tupleFromFoo, $fooFromTuple")

  // ネストした型
  val baz          = Baz(1, "b", 2.3, fooFromTuple)
  val tupleFromBaz = bazIso.to(baz)
  val bazFromTuple = bazIso.from(tupleFromBaz)
  println(s"$baz, $tupleFromBaz, $bazFromTuple")
}