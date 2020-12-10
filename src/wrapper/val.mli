type t =
  | Int32 of int
  | Int64 of int
  | Float32 of float
  | Float64 of float

val int_exn : t -> int
val float_exn : t -> float

module Kind : sig
  type t =
    | Int32
    | Int64
    | Float32
    | Float64
    | Any_ref
    | Func_ref
end

val kind : t -> Kind.t
