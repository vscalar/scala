package kuplrg

import javax.swing.text.html.HTML.Tag
import java.math.BigInteger

object Implementation extends Template {

  import Expr.*
  import Value.*

  type BOp[T] = (T, T) => T
  def numBOp(op: BOp[BigInt]): BOp[Value] = (_, _) match
    case (NumV(l), NumV(r)) => NumV(op(l, r))
    case _ => error("invalid operation")
  
  val numAdd: BOp[Value] = numBOp(_ + _)
  val numMul: BOp[Value] = numBOp(_ * _)
  
  def interp(expr: Expr, env: Env): Value = 
    //println(s"expr: $expr, env: $env");
    //println("")    
    expr match
    case Num(n) => NumV(n)
    case Add(l,r) => numAdd(interp(l, env), interp(r, env))
    case Mul(l,r) => numMul(interp(l, env), interp(r, env))
    case Val(x, e, b) => interp(b, env + (x -> interp(e, env)))
    case Id(x) => env.getOrElse(x, error("free identifier"))
    case App(f, e) => interp(f, env) match
      case CloV(p, b, fenv) => interp(b, fenv + (p -> interp(e, env)))
      case _ => error("not a function")
    case Fun(p, e) => CloV(p, e, env)

  def interpDS(expr: Expr, env: Env): Value =
    // println(s"expr: $expr, env: $env");
    // println("")
    expr match
    case Num(n) => NumV(n)
    case Add(l,r) => numAdd(interpDS(l, env), interpDS(r, env))
    case Mul(l,r) => numMul(interpDS(l, env), interpDS(r, env))
    case Val(x, e, b) => interpDS(b, env + (x -> interpDS(e, env)))
    case Id(x) => env.getOrElse(x, error("free identifier"))
    case App(f, e) => interpDS(f, env) match
      case CloV(p, b, fenv) => interpDS(b, env + (p -> interpDS(e, env)))
      case _ => error("not a function")
    case Fun(p, e) => CloV(p, e, env)
}
