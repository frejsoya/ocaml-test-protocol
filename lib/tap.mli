module Status : sig
    type t = Ok | NotOk [@@deriving show]
end

type tap_line =
  | Header of string
  | Plan of { count : int; reason : string option }
  | Test of {
      status : Status.t;
      id : int option;
      description : string option;
      comment : string option;
    }
  | Verbatim of string
  | Bail of string
[@@deriving show]


type tap_list = tap_line list [@@deriving show]


module Parser : sig
    module S :sig
        val text : (char -> bool) -> string option Angstrom.t
    end
  val tap_line : tap_line Angstrom.t
  val tap_lines : tap_line list Angstrom.t
  val read_all : string -> tap_line list
end
