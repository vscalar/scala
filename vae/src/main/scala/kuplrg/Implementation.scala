package kuplrg

object Implementation extends Template {

  import Expr.*

  def interp(expr: Expr, env: Env): Value = expr match
    case Num(n)   => n
    case Add(l,r) => interp(l, env) + interp(r, env)
    case Mul(l,r) => interp(l, env) * interp(r, env)
    case Val(x, e, b) => interp(b, env + (x -> interp(e, env)))
    case Id(x) => env.getOrElse(x, error("free identifier"))

  def freeIdsWithEnv(expr: Expr, env: Env): Set[String] = expr match
    case Num(_)   => Set()
    case Add(l,r) => freeIdsWithEnv(l, env) ++ freeIdsWithEnv(r, env)
    case Mul(l,r) => freeIdsWithEnv(l, env) ++ freeIdsWithEnv(r, env)
    case Val(x, e, b) => freeIdsWithEnv(e, env) ++ freeIdsWithEnv(b, env + (x -> 1))
    case Id(x) if (!env.contains(x)) => Set(x)
    case Id(_) => Set()

  def freeIds(expr: Expr): Set[String] = freeIdsWithEnv(expr, Map.empty)

  def bindingIds(expr: Expr): Set[String] = expr match
    case Num(n) => Set.empty[String]
    case Add(l,r) => bindingIds(l) ++ bindingIds(r)
    case Mul(l,r) => bindingIds(l) ++ bindingIds(r)
    case Val(x, e, b) => Set(x) ++ bindingIds(e) ++ bindingIds(b)
    case Id(x) => Set.empty[String]

  def boundIdsWithEnv(expr: Expr, env: Env): Set[String] = expr match
    case Num(n) => Set.empty[String]
    case Add(l,r) => boundIdsWithEnv(l, env) ++ boundIdsWithEnv(r, env)
    case Mul(l,r) => boundIdsWithEnv(l, env) ++ boundIdsWithEnv(r, env)
    case Val(x, e, b) => boundIdsWithEnv(e, env) ++ boundIdsWithEnv(b, env + (x -> 1))
    case Id(x) if (env.contains(x)) => Set(x)
    case Id(_) => Set.empty[String]

  def boundIds(expr: Expr): Set[String] = boundIdsWithEnv(expr, Map.empty)
    
  def shadowedIdsWithEnv(expr: Expr, env: Env): Set[String] = expr match
    case Num(_) => Set.empty[String]
    case Add(l,r) => shadowedIdsWithEnv(l, env) ++ shadowedIdsWithEnv(r, env)
    case Mul(l,r) => shadowedIdsWithEnv(l, env) ++ shadowedIdsWithEnv(r, env)
    case Val(x, e, b) if (env.contains(x))=> Set(x) ++ shadowedIdsWithEnv(e, env) ++ shadowedIdsWithEnv(b, env + (x -> 1))
    case Val(x, e, b) => shadowedIdsWithEnv(e, env) ++ shadowedIdsWithEnv(b, env + (x -> 1))
    case Id(_) => Set.empty[String]

  def shadowedIds(expr: Expr): Set[String] = shadowedIdsWithEnv(expr, Map.empty)

}
