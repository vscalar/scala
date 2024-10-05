package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  def interp(expr: Expr, env: Env): Value = expr match
    case Num(n) => n
    case Add(l,r) => interp(l, env) + interp(r, env = )
    case Mul(l,r) => interp(l, env) * interp(r, env)
    case Val(x, e, b) => interp(b, env + (x -> interp(e, env)))
    case Id(x) => env.getOrElse(x, error("free identifier"))
    case App(f, e) => 
    case Fun(p, e)

  def interpDS(expr: Expr, env: Env): Value = ???
}
