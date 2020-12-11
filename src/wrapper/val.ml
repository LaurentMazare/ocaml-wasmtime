open! Base

module T = struct
  type t =
    | Int32 of int
    | Int64 of int
    | Float32 of float
    | Float64 of float
end

include T

let int_exn = function
  | Int32 i | Int64 i -> i
  | Float32 f -> Printf.failwithf "expected an int, got f32 %f" f ()
  | Float64 f -> Printf.failwithf "expected an int, got f64 %f" f ()

let float_exn = function
  | Int32 i -> Printf.failwithf "expected a float, got i32 %d" i ()
  | Int64 i -> Printf.failwithf "expected a float, got i64 %d" i ()
  | Float32 i | Float64 i -> i

module Kind = struct
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

  type _ tuple =
    | T0 : unit tuple
    | T1 : 'a t -> 'a tuple
    | T2 : 'a t * 'b t -> ('a * 'b) tuple
    | T3 : 'a t * 'b t * 'c t -> ('a * 'b * 'c) tuple
    | T4 : 'a t * 'b t * 'c t * 'd t -> ('a * 'b * 'c * 'd) tuple
    | T5 : 'a t * 'b t * 'c t * 'd t * 'e t -> ('a * 'b * 'c * 'd * 'e) tuple

  let t0 = T0
  let t1 a = T1 a
  let t2 a b = T2 (a, b)
  let t3 a b c = T3 (a, b, c)
  let t4 a b c d = T4 (a, b, c, d)
  let t5 a b c d e = T5 (a, b, c, d, e)

  let arity : type a. a tuple -> int = function
    | T0 -> 0
    | T1 _ -> 1
    | T2 _ -> 2
    | T3 _ -> 3
    | T4 _ -> 4
    | T5 _ -> 5

  let pack_tuple (type a) (tuple : a tuple) =
    match tuple with
    | T0 -> []
    | T1 a -> [ P a ]
    | T2 (a, b) -> [ P a; P b ]
    | T3 (a, b, c) -> [ P a; P b; P c ]
    | T4 (a, b, c, d) -> [ P a; P b; P c; P d ]
    | T5 (a, b, c, d, e) -> [ P a; P b; P c; P d; P e ]

  let wrap_value (type a) (t : a t) (v : a) =
    match t with
    | Int32 -> T.Int32 v
    | Int64 -> Int64 v
    | Float32 -> Float32 v
    | Float64 -> Float64 v
    | Any_ref -> failwith "any_ref is not implemented"
    | Func_ref -> failwith "func_ref is not implemented"

  let unwrap_tuple : type a. a tuple -> T.t list -> a =
   fun tuple list ->
    let wrap_value : type b. b t -> T.t -> b =
     fun t v ->
      let type_mismatch ~expected =
        let type_ =
          match v with
          | Int32 _ -> "int32"
          | Int64 _ -> "int64"
          | Float32 _ -> "float32"
          | Float64 _ -> "float64"
        in
        Printf.failwithf "type mismatch: expected %s, got %s" expected type_ ()
      in
      match t, v with
      | Int32, Int32 i -> i
      | Int32, _ -> type_mismatch ~expected:"int32"
      | Int64, Int64 i -> i
      | Int64, _ -> type_mismatch ~expected:"int64"
      | Float32, Float32 f -> f
      | Float32, _ -> type_mismatch ~expected:"float32"
      | Float64, Float64 f -> f
      | Float64, _ -> type_mismatch ~expected:"float64"
      | Any_ref, _ -> failwith "any_ref is not implemented"
      | Func_ref, _ -> failwith "func_ref is not implemented"
    in
    let len_mismatch ~expected =
      Printf.failwithf
        "size mismatch: expected %d elements, got %d"
        expected
        (List.length list)
        ()
    in
    match tuple, list with
    | T0, [] -> ()
    | T0, _ -> len_mismatch ~expected:0
    | T1 t, [ v ] -> wrap_value t v
    | T1 _, _ -> len_mismatch ~expected:1
    | T2 (t0, t1), [ v0; v1 ] -> wrap_value t0 v0, wrap_value t1 v1
    | T2 _, _ -> len_mismatch ~expected:2
    | T3 (t0, t1, t2), [ v0; v1; v2 ] ->
      wrap_value t0 v0, wrap_value t1 v1, wrap_value t2 v2
    | T3 _, _ -> len_mismatch ~expected:3
    | T4 (t0, t1, t2, t3), [ v0; v1; v2; v3 ] ->
      wrap_value t0 v0, wrap_value t1 v1, wrap_value t2 v2, wrap_value t3 v3
    | T4 _, _ -> len_mismatch ~expected:4
    | T5 (t0, t1, t2, t3, t4), [ v0; v1; v2; v3; v4 ] ->
      ( wrap_value t0 v0
      , wrap_value t1 v1
      , wrap_value t2 v2
      , wrap_value t3 v3
      , wrap_value t4 v4 )
    | T5 _, _ -> len_mismatch ~expected:5

  let wrap_tuple (type a) (tuple : a tuple) (v : a) =
    match tuple with
    | T0 -> []
    | T1 t -> [ wrap_value t v ]
    | T2 (t0, t1) ->
      let v0, v1 = v in
      [ wrap_value t0 v0; wrap_value t1 v1 ]
    | T3 (t0, t1, t2) ->
      let v0, v1, v2 = v in
      [ wrap_value t0 v0; wrap_value t1 v1; wrap_value t2 v2 ]
    | T4 (t0, t1, t2, t3) ->
      let v0, v1, v2, v3 = v in
      [ wrap_value t0 v0; wrap_value t1 v1; wrap_value t2 v2; wrap_value t3 v3 ]
    | T5 (t0, t1, t2, t3, t4) ->
      let v0, v1, v2, v3, v4 = v in
      [ wrap_value t0 v0
      ; wrap_value t1 v1
      ; wrap_value t2 v2
      ; wrap_value t3 v3
      ; wrap_value t4 v4
      ]
end

let kind = function
  | Int32 _ -> Kind.P Int32
  | Int64 _ -> P Int64
  | Float32 _ -> P Float32
  | Float64 _ -> P Float64
