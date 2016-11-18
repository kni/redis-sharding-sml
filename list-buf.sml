structure ListBuf = 
struct

datatype ListBuf = LB of int * string list list

val empty = LB(0, [[]])

fun size (LB(s, _)) = s

fun add (LB(s, l)) added = 
  let
    val added_size = List.foldl (fn(a,s) => s + String.size(a)) 0 added
  in
    LB(added_size + s, added::l)
  end

fun toString (LB(s, l)) = String.concat(List.concat(List.rev l))

end


(*
open ListBuf;
val e = empty
val sb = add (add (add e ["a", "b"]) ["s", "d"]) ["e", "f"]
val s = toString sb
val _ = print s
*)
