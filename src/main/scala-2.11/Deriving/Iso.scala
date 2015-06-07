package Deriving

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait Iso[T, U] {
  def to(t: T): U

  def from(u: U):T
}

object Iso {
  implicit def materializeIso[T, U]: Iso[T, U] = macro impl[T, U]

  def impl[T: c.WeakTypeTag, U: c.WeakTypeTag](c: whitebox.Context): c.Expr[Iso[T, U]] = {
    import c.universe._

    // case classのクラス名
    val caseClassSym: c.universe.Symbol = c.weakTypeOf[T].typeSymbol
    if (!caseClassSym.isClass || !caseClassSym.asClass.isCaseClass) c.abort(c.enclosingPosition, s"$caseClassSym is not a case class")

    // case classのフィールドに関するobject
    object Fields {
      // 各フィールドのシンボル
      val syms: List[TermSymbol] = caseClassSym.typeSignature.decls.toList.collect { case x: TermSymbol if x.isVal && x.isCaseAccessor => x }

      // List(Int, String, Int) のようなフィールドの型名を並べたリスト
      val types: List[Tree] = syms map (f => tq"${f.typeSignature}")

      // Lost(t.foo, t.bar, t.baz) のように 「t.フィールド名」 と並べたリスト (tはtoで使えるように、toの引数名に合わせている)
      // 本当はt.nameとしたいが、t.`foo` となってしまうので、　t.fooにするためにstring化→trimして回避している
      val names: List[Tree] = syms map (f => q"t.${TermName(f.name.toString.trim)}")
    }

    import Fields._

    // case classと同型なタプルの型
    // Tuple3[Int, String, Int]など
    def tupleType: Tree = tq"(..$types)"

    // case classと同型なタプルのインスタンス
    // (1, "aaa", 2)など
    def toImpl: Tree = q"(..$names)"

    // タプルと同型なcase classのインスタンス
    // case classのフィールドが0個のときはapply.tupledが無いので直接インスタンス化する
    // Foo(1, "aaa" 2)など
    def fromImpl: Tree = q"${if(syms.isEmpty) q"${caseClassSym.companion}.apply" else q"${caseClassSym.companion}.tupled(u)"}"

    // Iso[T, U]の無名サブクラスを定義してnew
    val evidence: Tree = q"""
      final class $$anon extends Iso[$caseClassSym, $tupleType] {
        def to(t: $caseClassSym): $tupleType = $toImpl
        def from(u: $tupleType): $caseClassSym = $fromImpl
      }
      new $$anon
    """

    c.Expr[Iso[T, U]](evidence)
  }
}