type 'a t = {
  mutable siz : int;
  mutable arr : ('a option) array
}

let create_empty () =
  { siz = 0; arr = Array.make 4 None }
let init siz f = {
  siz;
  arr = Array.init siz (fun x -> Some (f x))
}
let make siz v = init siz (fun _ -> v)

let get a pos =
  match a.arr.(pos) with
  | None -> raise @@ Invalid_argument ""
  | Some x -> x
let set a pos x =
  a.arr.(pos) <- Some x

let grow ({siz; arr} as p) =
  let nsiz = siz * 2 in
  let narr = Array.init nsiz (fun x -> if x < siz then arr.(x) else None) in
  p.arr <- narr
let new_pos p =
  if p.siz = Array.length p.arr then
    grow p
  else
    ();
  let ret = p.siz in
  p.siz <- ret + 1;
  ret
let push_back p v = 
  let pos = new_pos p in
  set p pos v