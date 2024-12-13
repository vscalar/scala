package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def typeCheck(
    expr: Expr,
    tenv: TypeEnv,
    sol: Solution,
  ): (Type, Solution) = ???

  def interp(expr: Expr, env: Env): Value = ???
}
