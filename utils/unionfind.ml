type 'a t = {
  fa : int array;
  rk : int array;
  content : 'a array
}

let init s init_content =
  {
    fa = Array.init s (fun x -> x);
    rk = Array.make s 1;
    content = Array.init s init_content
  }

let rec get_fa uf x =
  if uf.fa.(x) = x then
    x
  else
    let res = get_fa uf @@ uf.fa.(x) in
    uf.fa.(x) <- res;
    res

let is_same uf x y =
  get_fa uf x = get_fa uf y

let link_set uf son father =
  uf.fa.(son) <- father;
  let mid = uf.rk.(father) in
  if uf.rk.(son) = mid then
    uf.rk.(father) <- mid + 1
  else
    ()
let merge_set uf x y =
  if is_same uf x y then
    ()
  else
    let a = get_fa uf x in
    let b = get_fa uf y in
    if uf.rk.(a) > uf.rk.(b) then
      link_set uf b a
    else
      link_set uf a b

let get uf x =
  Array.get uf.content @@ get_fa uf x
let set uf x v = 
  uf.content.(get_fa uf x) <- v