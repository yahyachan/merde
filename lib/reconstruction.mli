exception Type_recursion of int * Syntax.ty
(** 类型出现自我递归 *)

exception Type_mismatch of Syntax.ty * Syntax.ty
(** 类型不匹配 *)

val reconstruct_toplevel :
  Syntax.poly_ty Syntax.Env.t -> Syntax.term -> Syntax.poly_ty
(** [reconstrct env tm] 在 [env] 这个类型上下文里对表达式 [tm] 进行推导，
    给出最优解，出错的情况下通过异常报错 *)