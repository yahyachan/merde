type 'a t = { mutable siz : int; mutable arr : 'a option array; }
(* 表示内容为 'a 类型的可变长数组接口 *)

val create_empty : unit -> 'a t
(* 建立一个新的空可变长数组 *)

val init : int -> (int -> 'a) -> 'a t
(* [init n f] 建立一个长度为 [n] 的可变长数组，其中第 [i] 个位置
   值为 [f i] *)

val make : int -> 'a -> 'a t
(* [make n a] 建立一个长度为 [n] 的可变长数组，其内容全为 [a] *)

val get : 'a t -> int -> 'a
(* [get arr pos] 获取数组 [arr] 第 [pos] 位置的内容 *)

val set : 'a t -> int -> 'a -> unit
(* [set arr pos v] 将数组 [arr] 第 [pos] 位置内容设为 [v] *)

val new_pos : 'a t -> int
(* [new_pos arr] 将数组大小增大一 *)

val push_back : 'a t -> 'a -> unit
(* [push_back] 在数组尾部塞入一个新元素 *)