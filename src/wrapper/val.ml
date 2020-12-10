open! Base

type t =
  | Int32 of int
  | Int64 of int
  | Float32 of float
  | Float64 of float

let int_exn = function
  | Int32 i | Int64 i -> i
  | Float32 f -> Printf.failwithf "expected an int, got f32 %f" f ()
  | Float64 f -> Printf.failwithf "expected an int, got f64 %f" f ()

let float_exn = function
  | Int32 i -> Printf.failwithf "expected a float, got i32 %d" i ()
  | Int64 i -> Printf.failwithf "expected a float, got i64 %d" i ()
  | Float32 i | Float64 i -> i

module Kind = struct
  type t =
    | Int32
    | Int64
    | Float32
    | Float64
    | Any_ref
    | Func_ref
end

let kind = function
  | Int32 _ -> Kind.Int32
  | Int64 _ -> Kind.Int64
  | Float32 _ -> Kind.Float32
  | Float64 _ -> Kind.Float64
