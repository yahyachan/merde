type 'a t = { fa : int Vector.t; rk : int Vector.t; content : 'a Vector.t; }
(* 并查集类型 *)

val init : int -> (int -> 'a) -> 'a t
(* [init n f] 建立新的大小为 [n] 的并查集，每个位置 [i] 的内容值为 [f i] *)

val new_var : 'a t -> (int -> 'a) -> int
(* [new_var uf f] 在并查集 [uf] 中建立一个孤立的新集合，其内容为 [f] 带入其下标 *)

val get_fa : 'a t -> int -> int
(* [get_fa uf pos] 获取集合 [pos] 在并查集中的根 *)

val is_same : 'a t -> int -> int -> bool
(* [is_same uf x y] 查询在并查集 [uf] 中 [x] 与 [y] 是否等价 *)

val merge_set : 'a t -> int -> int -> unit
(* [merge_set uf x y] 在并查集 [uf] 中合并 [x] 和 [y] *)

val get : 'a t -> int -> 'a
(* [get uf x] 查询 [x] 代表集合的内容值 *)

val set : 'a t -> int -> 'a -> unit
(* [set uf x v] 设置 [x] 代表集合的内容值为 [v] *)