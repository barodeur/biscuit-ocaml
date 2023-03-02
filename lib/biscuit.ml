exception NotImplementedYet

let ni () = raise NotImplementedYet

module Key = struct
  type t
end

module KeyPair = struct
  type t

  let make () = ni ()
  let public _pair = ni ()
end

module Authorizer = struct
  type t

  let make _Str = ni ()
end

type t

let make _str = ni ()
let append_string _str = ni ()
let authorize _biscuit _authorizer = ni ()
let to_bytes _biscuit = ni ()
let of_bytes _bytes ~key:_ = ni ()
