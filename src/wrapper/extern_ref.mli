type t

val of_string : string -> t

module Private : sig
  val to_val : t -> Import.W.Val.t
  val of_val : Import.W.Val.t -> t
end
