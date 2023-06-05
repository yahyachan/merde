exception Var_not_found of string *
            (Lexing.position * Lexing.position) option
(** 变量未找到的错误 *)

exception Runtime_type_error
(** 运行时类型错误 *)

val eval : Syntax.value Syntax.Env.t -> Syntax.term -> Syntax.value
(** [eval env tm] 在上下文 [env] 中解释程序 [tm]，成功则返回求得值，
    失败则用异常的形式报错 *)