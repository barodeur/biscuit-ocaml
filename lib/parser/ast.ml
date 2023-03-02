open Core

module DateTime = struct
  type t = Ptime.t

  let sexp_of_t time = Sexp.Atom (Ptime.to_rfc3339 time)

  let t_of_sexp = function
    | Sexp.Atom str ->
        Ptime.of_rfc3339 str |> Result.ok |> Option.value_exn |> Tuple3.get1
    | _ -> invalid_arg "invalid"
end

module Set = struct
  module Term = struct
    type t =
      | String of string
      | Number of int
      | Bool of bool
      | Hex of string
      | DateTime of DateTime.t
    [@@deriving sexp]
  end

  type t = Term.t list [@@deriving sexp]
end

module Fact = struct
  module Term = struct
    type t =
      | String of string
      | Number of int
      | Bool of bool
      | Hex of string
      | DateTime of DateTime.t
      | Set of Set.t
    [@@deriving sexp]
  end

  type t = { name : string; terms : Term.t list } [@@deriving sexp]
end

module Term = struct
  type t = Fact of Fact.Term.t | Variable of string [@@deriving sexp]
end

module Predicate = struct
  type t = { name : string; terms : Term.t list } [@@deriving sexp]
end

module Operator = struct
  type t = Equal | GreaterThan | GreaterEqualThan | LowerThan | LowerEqualThan
  [@@deriving sexp]
end

module Expression = struct
  module MethodCall = struct
    type t = { name : string; args : Term.t } [@@deriving sexp]
  end

  type term = Term of Term.t | Expression of t [@@deriving sexp]
  and t = element * (Operator.t * element) list

  and element =
    | Unary of t
    | ElementTerm of { term : term; method_call : MethodCall.t option }
  [@@deriving sexp]
end

module Rule = struct
  module Body = struct
    type element = Predicate of Predicate.t | Expression of Expression.t
    [@@deriving sexp]

    type t = { elements : element list } [@@deriving sexp]
  end

  type t = { head : Predicate.t; body : Body.t } [@@deriving sexp]
end

module Policy = struct
  type action = Allow | Deny [@@deriving sexp]
  type t = { action : action; queries : Rule.Body.t list } [@@deriving sexp]
end

module Check = struct
  type kind = One | All [@@deriving sexp]
  type t = { kind : kind; queries : Rule.Body.t list } [@@deriving sexp]
end

module Block = struct
  type element = Check of Check.t | Fact of Fact.t | Rule of Rule.t
  [@@deriving sexp]

  type t = element list [@@deriving sexp]

  let to_string block = sexp_of_t block |> Sexp.to_string
  let print block = to_string block |> print_string
end

module Authorizer = struct
  type element = Policy of Policy.t | Check of Check.t [@@deriving sexp]
  type t = element list [@@deriving sexp]

  let to_string block = sexp_of_t block |> Sexp.to_string
  let print block = to_string block |> print_string
end
