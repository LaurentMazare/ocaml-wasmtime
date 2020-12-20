module T : sig
  type t =
    | Int32 of int
    | Int64 of int
    | Float32 of float
    | Float64 of float
    | Extern_ref of Extern_ref.t
end

include module type of T with type t = T.t

val int_exn : t -> int
val float_exn : t -> float

module Kind : sig
  type any_ref
  type func_ref

  type 'a t =
    | Int32 : int t
    | Int64 : int t
    | Float32 : float t
    | Float64 : float t
    | Any_ref : any_ref t
    | Func_ref : func_ref t

  type packed = P : 'a t -> packed
  type _ tuple

  val t0 : unit tuple
  val t1 : 'a t -> 'a tuple
  val t2 : 'a t -> 'b t -> ('a * 'b) tuple
  val t3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) tuple
  val t4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) tuple
  val t5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) tuple
  val arity : _ tuple -> int
  val pack_tuple : 'a tuple -> packed list
  val unwrap_tuple : 'a tuple -> T.t list -> 'a
  val wrap_tuple : 'a tuple -> 'a -> T.t list
end

val kind : t -> Kind.packed
