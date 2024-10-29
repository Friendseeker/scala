package example
import scala.language.experimental.macros
import scala.reflect.macros._

object Provider {
  def tree(args: Any): Any = macro treeImpl
  def treeImpl(c: Context)(args: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._
    val hasField = weakTypeOf[A].members.exists {
      case m: TermSymbol => m.isVal || m.isVar
      case _ => false
    }
    if (hasField) {
      reify(args.splice)
    } else {
      c.Expr[Boolean](Literal(Constant(hasField)))
    }
  }
}