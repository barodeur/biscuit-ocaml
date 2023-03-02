module Key : sig
  type t
end

module KeyPair : sig
  type t

  val make : unit -> t
  val public : t -> Key.t
end

module Authorizer : sig
  type t

  val make : string -> Key.t
end

type t

val make : string -> KeyPair.t -> t
val to_bytes : t -> bytes
val of_bytes : bytes -> key:Key.t -> t
val append_string : t -> string -> t
val authorize : t -> Authorizer.t -> (string, string) result
