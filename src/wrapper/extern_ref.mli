type t

val of_string : string -> t

module Private : sig
  val ptr : t -> Import.W.Val.t
end
