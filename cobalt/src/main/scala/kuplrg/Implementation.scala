package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  // ---------------------------------------------------------------------------
  // Problem #1
  // ---------------------------------------------------------------------------
  def interp(expr: Expr, env: Env): Value = ???

  def eq(left: Value, right: Value): Boolean = ???

  def map(list: Value, func: Value): Value = ???

  def join(list: Value): Value = ???

  def filter(list: Value, func: Value): Value = ???

  def foldLeft(list: Value, init: Value, func: Value): Value = ???

  def app(func: Value, args: List[Value]): Value = ???

  // ---------------------------------------------------------------------------
  // Problem #2
  // ---------------------------------------------------------------------------
  def subExpr1: String = ???

  def subExpr2: String = ???
}
