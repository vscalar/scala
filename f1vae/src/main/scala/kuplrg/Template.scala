package kuplrg

trait Template {

  def eval(str: String): String =
    val program: Program = Program(str)
    val fenv: FEnv = createFEnv(program.fdefs)
    interp(program.expr, Map.empty, fenv).toString

  def evalDS(str: String): String =
    val program: Program = Program(str)
    val fenv: FEnv = createFEnv(program.fdefs)
    interpDS(program.expr, Map.empty, fenv).toString

  def createFEnv(fdefs: List[FunDef]): FEnv =
    fdefs.foldLeft(Map.empty)((fenv: FEnv, fdef: FunDef) => {
      val fname: String = fdef.name
      if (fenv.contains(fname)) error(s"duplicate function: $fname")
      else fenv + (fname -> fdef)
    })

  def interp(expr: Expr, env: Env, fenv: FEnv): Value

  def interpDS(expr: Expr, env: Env, fenv: FEnv): Value
}
