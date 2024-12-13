package kuplrg

trait Template {

  def eval(str: String): String =
    val expr = Expr(str)
    val t = TypeScheme.from(typeCheck(expr, Map.empty, Map.empty))
    val v = interp(expr, Map.empty)
    s"${v.str}: ${t.str}"

  def typeCheck(
    expr: Expr,
    tenv: TypeEnv,
    sol: Solution,
  ): (Type, Solution)

  def interp(expr: Expr, env: Env): Value

}
