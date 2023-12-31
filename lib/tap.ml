(* Exampel input
   KTAP version 1
   1..2
     KTAP version 1
     1..2
       KTAP version 1
       1..2
       not ok 1 test_1
       ok 2 test_2
     not ok 1 test_3
     ok 2 test_4 # SKIP
   not ok 1 example_test_1
   ok 2 example_test_2
*)


type status = Ok | NotOk [@@deriving show]
type tap  =
  | Header of string
  | Plan of { count : int; reason : string option }
  | Test of {
      status : status;
      id : int option;
      description : string option;
      comment : string option;
    }
  | Verbatim of string
  (* Bail out *)
[@@deriving show]

module Parser = struct
  open Angstrom

  (* Streaming parser *)
  let is_digit = function '0' .. '9' -> true | _ -> false
  let tap_ok = string "ok" *> return Ok
  let tap_notok = string "not ok" *> return NotOk
  let whitespace = skip_while (fun c -> c = ' ' || c = '\t')
  let digits = take_while1 is_digit >>| int_of_string
  let comment = char '#' *> Angstrom.take_while (fun c -> c <> '\n')
  let tap_header = string "TAP Version 14"
  let status = tap_ok <|> tap_notok <?> "Parsing status <ok | not ok>"

  (** [opt p] returns [Some p] if [p] succees or [None] if p fails*)
  let opt p = option None (p >>| Option.some)

  (* can be followed by a comment *)
  let plan =
    let reason = whitespace *> opt comment in
    lift2
      (fun count reason -> Plan { count; reason })
      (string "1.." *> digits)
      reason

  let description_end = function '#' -> true | '\n' -> true | _ -> false
  let test_id = char ' ' *> digits
  let test_desc = string " - " *> take_while (fun c -> not @@ description_end c)
  let test_directive = whitespace *> comment

  let status_line =
    lift4
      (fun status id description comment ->
        Test { status; id; description; comment })
      status (* ok | not ok *)
      (opt test_id) (* digit *)
      (opt test_desc) (* ' - ' text *)
      (opt test_directive)
  (*'# comment'*)

  let tap =
    let* char = peek_char in
    match char with
    | Some 'T' ->
        let+ h = tap_header in
        Header h
    | Some '#' ->
        let+ c = comment in
        Verbatim c
    | Some '1' -> plan
    | Some 'o' | Some 'n' -> status_line <?> "Parsing status line"
    | Some other -> fail (Printf.sprintf "Unknown char at pos ? %c" other)
    | None -> fail "FIXME unexpected end of input"

  (* let body = many_till body end_of_input *)
  let read_all (str : string) =
    match Angstrom.parse_string ~consume:All tap str with
    | Ok v -> v
    | Error msg -> failwith msg
end

let print_tap_line l = l |> show_tap |> print_endline

let%expect_test "version" =
  Parser.read_all "TAP Version 14" |> print_tap_line;
  [%expect {| (Tap.Header "TAP Version 14") |}]

let%expect_test "plan line" =
  Parser.read_all "1..5" |> print_tap_line;
  [%expect {| Tap.Plan {count = 5; reason = None} |}];

  Parser.read_all "1..0 # reason" |> print_tap_line;
  [%expect {| Tap.Plan {count = 0; reason = (Some " reason")} |}]

let%expect_test "Status line" =
  (* Status line *)
  Parser.read_all "ok" |> print_tap_line;
  [%expect
    {|
    Tap.Test {status = Tap.Ok; id = None; description = None; comment = None} |}];

  Parser.read_all "not ok" |> print_tap_line;
  [%expect
    {|
    Tap.Test {status = Tap.NotOk; id = None; description = None; comment = None} |}];

  Parser.read_all "ok 1" |> print_tap_line;
  print_endline "";
  Parser.read_all "not ok 1" |> print_tap_line;
  [%expect
    {|
    Tap.Test {status = Tap.Ok; id = (Some 1); description = None; comment = None}

    Tap.Test {status = Tap.NotOk; id = (Some 1); description = None;
      comment = None} |}]

let%expect_test "status line : title" =
  Parser.read_all "ok 1 - foo" |> print_tap_line;
  Parser.read_all "not ok 1 - bar" |> print_tap_line;
  [%expect
    {|
    Tap.Test {status = Tap.Ok; id = (Some 1); description = (Some "foo");
      comment = None}
    Tap.Test {status = Tap.NotOk; id = (Some 1); description = (Some "bar");
      comment = None} |}]

let%expect_test "status line : parse description" =
  Parser.read_all "ok 1 - foo" |> print_tap_line;
  Parser.read_all "not ok 1 - bar" |> print_tap_line;
  [%expect
    {|
  Tap.Test {status = Tap.Ok; id = (Some 1); description = (Some "foo");
    comment = None}
  Tap.Test {status = Tap.NotOk; id = (Some 1); description = (Some "bar");
    comment = None} |}];

  Parser.read_all "ok - foo" |> print_tap_line;
  Parser.read_all "not ok - foo" |> print_tap_line;
  [%expect
    {|
    Tap.Test {status = Tap.Ok; id = None; description = (Some "foo");
      comment = None}
    Tap.Test {status = Tap.NotOk; id = None; description = (Some "foo");
      comment = None} |}]

let%expect_test "status line : parse comment" =
(*
Combinations: 2^3
ok | not ok
with id  | without i
with title | without title
*)
  Parser.read_all "ok 1 - foo #SKIP: lazy engineer" |> print_tap_line;

  [%expect
    {|
    Tap.Test {status = Tap.Ok; id = (Some 1); description = (Some "foo ");
      comment = (Some "SKIP: lazy engineer")} |}];



  Parser.read_all "ok 1 #SKIP: missing title" |> print_tap_line;
  [%expect {|
    Tap.Test {status = Tap.Ok; id = (Some 1); description = None;
      comment = (Some "SKIP: missing title")} |}];


Parser.read_all "ok #SKIP: missing id+title" |> print_tap_line;
[%expect {|
  Tap.Test {status = Tap.Ok; id = None; description = None;
    comment = (Some "SKIP: missing id+title")} |}];
