module Env :
  sig
    type key = string
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end
type ty =
    TInt
  | TBool
  | TFun of ty * ty
  | TVar of int
  | TRowEmpty
  | TRowExtension of ty list Env.t * row
  | TRecord of row
  | TVariants of row
and row = ty
(** 类型以及行类型，为了方便将二者一视同仁 *)

val find_row_tail : row -> int option
(** 找到一个行类型的尾变量，没有则返回空 *)

val insert_lenv : 'a list Env.t -> Env.key -> 'a -> 'a list Env.t
(** 在列表型映射（二叉搜索树）中快速插入一个值 *)

type poly_ty = PolyType of int list * row
(** 带有泛型的类型 *)

type varname = string
(** 变量名 *)

val concat_env : 'a list Env.t -> 'a list Env.t -> 'a list Env.t
(** 将两个列表型映射（二叉搜索树）合并 *)

type binop = Plus | Sub | Times | Div | Equal | Less
(** 二元运算符 *)

type term' =
    Int of int
  | Bool of bool
  | Var of varname
  | Binop of binop * term * term
  | If of term * term * term
  | Fun of varname * term
  | Let of varname * term * term
  | Apply of term * term
  | Fix of varname * term
  | RecordSelect of term * varname
  | RecordExtension of term list Env.t * term
  | RecordRestrict of term * varname
  | RecordEmpty
  | Variant of varname * term
  | Match of term * (pattern * term) list
(** 表达式类型 *)

and term = { e : term'; pos : (Lexing.position * Lexing.position) option; }
(** 附加上表达式位置信息的表达式类型 *)

and pattern = PVar of varname | PVariant of varname * pattern
(** 匹配类型 *)


type value =
    VInt of int
  | VBool of bool
  | VClosure of value Env.t * varname * term
  | VFix of value Env.t * varname * term *
      (Lexing.position * Lexing.position) option
  | VRecord of value list Env.t
  | VLabel of varname * value
(** 可能的值 *)

type command = TopTerm of term | TopDef of varname * term | TopExit
(** 表示命令的类型 *)

module For_test :
  sig
    val fill : term' -> term
    val eint : int -> term
    val ebool : bool -> term
    val var : varname -> term
    val efun : varname -> term -> term
    val lfun : varname list -> term -> term
    val eif : term -> term -> term -> term
    val elet : varname -> term -> term -> term
    val apply : term -> term -> term
    val lapply : term -> term list -> term
    val efix : varname -> term -> term
    val select : term -> varname -> term
    val rempty : term
    val ext : term list Env.t -> term -> term
    val erecord : term list Env.t -> term
    val remove : term -> varname -> term
    val evariant : varname -> term -> term
    val ematch : term -> (pattern * term) list -> term
    module Infix :
      sig
        val ( + ) : term -> term -> term
        val ( - ) : term -> term -> term
        val ( * ) : term -> term -> term
        val ( / ) : term -> term -> term
        val ( = ) : term -> term -> term
        val ( < ) : term -> term -> term
      end
  end
(** 用于方便维护的一个模块（见本文档6.3节所述） *)

val string_of_value : value -> string
(** 将值转为字符串以显示给用户 *)

val string_of_polytype : poly_ty -> string
(** 将有泛型的类型转换成字符串显示给用户 *)

val string_of_type : ty -> string
(** 将没有现式泛型的类型转换成字符串显示给用户 *)