type status = Ok | NotOk [@@deriving show]

type tap_line =
  | Header of string
  | Plan of { count : int; reason : string option }
  | Test of {
      status : status;
      id : int option;
      description : string option;
      comment : string option;
    }
  | Verbatim of string
  | Bail of string
[@@deriving show]

type tap = tap_line list
[@@deriving show]



module Parser : sig
    module S :sig
        val text : (char -> bool) -> string Angstrom.t
    end
  val tap_line : tap_line Angstrom.t
  val tap : tap Angstrom.t
  val read_all : string -> tap
end
