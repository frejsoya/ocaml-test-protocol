type status = Ok | NotOk [@@deriving show]

type tap =
  | Header of string
  | Plan of { count : int; reason : string option }
  | Test of {
      status : status;
      id : int option;
      description : string option;
      comment : string option;
    }
  | Verbatim of string
[@@deriving show]

module Parser : sig
  val tap : tap Angstrom.t
  val read_all : string -> tap
end
