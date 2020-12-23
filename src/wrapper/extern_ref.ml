open! Base
open! Import

type t = W.Val.t

let of_string str =
  let t = Ctypes.allocate_n W.Val.struct_ ~count:1 in
  W.Val.extern_ref str t;
  Caml.Gc.finalise W.Val.delete t;
  t

module Private = struct
  let to_val = Fn.id
  let of_val = Fn.id
end
