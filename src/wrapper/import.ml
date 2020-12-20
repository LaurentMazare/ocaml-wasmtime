open! Base
module W = Bindings.C (Wasmtime_generated)

exception Trap of { message : string }

(* Taken from [Core_kernel.Gc]. *)
let zero = Sys.opaque_identity (Caml.int_of_string "0")

(* The compiler won't optimize int_of_string away so it won't
   perform constant folding below. *)
let rec keep_alive o = if zero <> 0 then keep_alive (Sys.opaque_identity o)
