type 'a t = {
  fa : int Vector.t;
  rk : int Vector.t;
  content : 'a Vector.t
}

let init s init_content =
  {
    fa = Vector.init s (fun x -> x);
    rk = Vector.make s 1;
    content = Vector.init s init_content
  }

let new_var uf f =
  let pos = Vector.new_pos uf.fa in
  Vector.set uf.fa pos pos;
  Vector.push_back uf.rk 1;
  Vector.push_back uf.content @@ f pos;
  pos

let rec get_fa uf x =
  if Vector.get uf.fa x = x then
    x
  else
    let res = get_fa uf @@ Vector.get uf.fa x in
    Vector.set uf.fa x res;
    res

let is_same uf x y =
  get_fa uf x = get_fa uf y

let link_set uf son father =
  Vector.set uf.fa son father;
  let mid = Vector.get uf.rk (father) in
  if Vector.get uf.rk (son) = mid then
    Vector.set uf.rk (father) @@ mid + 1
  else
    ()
let merge_set uf x y =
  if is_same uf x y then
    ()
  else
    let a = get_fa uf x in
    let b = get_fa uf y in
    if Vector.get uf.rk (a) > Vector.get uf.rk (b) then
      link_set uf b a
    else
      link_set uf a b

let get uf x =
  Vector.get uf.content @@ get_fa uf x
let set uf x v = 
  Vector.set uf.content (get_fa uf x) v