[@@@warning "-32"]

module Status = struct
  type t = Ok | NotOk [@@deriving show]
end

type tap_line =
  | Header of string
  | Plan of {count: int; reason: string option}
  | Test of
      { status: Status.t
      ; id: int option
      ; description: string option
      ; comment: string option }
  | Verbatim of string (* Ideally fold verbatim into one blob *)
  | Bail of string
[@@deriving show]

type tap_list = tap_line list [@@deriving show]

module Parser = struct
  open Angstrom

  module S = struct
    type t = [`Unescaped | `Escaped | `Invalid of char]

    let unescaped terminal buf = function
      | '\\' ->
          Some (`Escaped, buf)
      | c when terminal c ->
          None (* Terminal *)
      | c ->
          Buffer.add_char buf c ;
          Some (`Unescaped, buf)

    let escaped terminal buf = function
      | '\\' as c ->
          Buffer.add_char buf c ;
          Some (`Unescaped, buf)
      | c when terminal c ->
          Buffer.add_char buf c ;
          Some (`Unescaped, buf)
      | c ->
          Buffer.add_char buf c (* For error message*) ;
          Some (`Invalid c, buf)

    let text terminal =
      let buf = Buffer.create 64 in
      let* (matched, (state, buf)) : string * (t * Buffer.t) =
        scan (`Unescaped, buf) (fun (state, buf) c ->
            match (state, c) with
            | `Invalid _inv_char, _ ->
                None
            | `Unescaped, '\\' ->
                Some (`Escaped, buf)
            | `Unescaped, c when terminal c ->
                None (* Terminal *)
            | `Unescaped, c ->
                Buffer.add_char buf c ;
                Some (`Unescaped, buf)
            | `Escaped, '\\' ->
                Buffer.add_char buf c ;
                Some (`Unescaped, buf)
            | `Escaped, c when terminal c ->
                (* Allowed escape char *)
                Buffer.add_char buf c ;
                Some (`Unescaped, buf)
            | `Escaped, c ->
                Buffer.add_char buf c (* For error message*) ;
                Some (`Invalid c, buf) )
      in
      let buf_str = Buffer.to_bytes buf |> String.of_bytes in
      Buffer.clear buf ;
      (* `Empty | `Invalid of char | `Valid of string *)
      match (state, String.trim matched) with
      | `Unescaped, "" ->
          return None
      | `Unescaped, _ ->
          return (Some buf_str)
      | `Invalid c, _matched ->
          let msg = Printf.sprintf "Invalid escaped char '%c'" c in
          fail msg
      | `Escaped, _matched ->
          (* Unable to continue reading past escape char. Eol? *)
          let msg = Printf.sprintf "invalid string \"%s\"" buf_str in
          fail msg
  end

  let is_digit = function '0' .. '9' -> true | _ -> false

  let digits = take_while1 is_digit >>| int_of_string

  let whitespace = skip_while (fun c -> c = ' ' || c = '\t')

  let tap_header = string "TAP Version 14"

  let tap_ok = string "ok" *> return Status.Ok

  let tap_notok = string "not ok" *> return Status.NotOk

  let comment_to_eol = char '#' *> Angstrom.take_while (fun c -> c <> '\n')

  let status = tap_ok <|> tap_notok <?> "Parsing status <ok | not ok>"

  (** [opt p] returns [Some p] if [p] succees or [None] if p fails*)
  let opt p = option None (p >>| Option.some)

  (* can be followed by a comment *)
  let plan =
    let reason = whitespace *> opt comment_to_eol in
    lift2
      (fun count reason -> Plan {count; reason})
      (string "1.." *> digits)
      reason

  let description_end = function '#' -> true | '\n' -> true | _ -> false

  let test_description =
    let* _ = skip_while (fun c -> c == ' ' || c == '-') in
    S.text description_end <?> "parsing test point description"

  let test_directive = whitespace *> comment_to_eol

  let status_line =
    let id = opt @@ (char ' ' *> digits) in
    let+ status = status
    and+ id = id
    and+ description = test_description
    and+ comment = opt test_directive in
    Test {status; id; description; comment}

  let tap_line =
    let* char = peek_char in
    match char with
    | Some 'T' ->
        let+ h = tap_header in
        Header h
    | Some '#' ->
        let+ c = comment_to_eol in
        Verbatim c
    | Some '1' ->
        let+ plan = plan in
        plan
    | Some 'o' | Some 'n' ->
        let status_line = status_line <?> "Parsing status line" in
        status_line
    | Some other ->
        fail (Printf.sprintf "Unknown char at pos ? %c" other)
    | None ->
        fail "could not parse tap_line "

  (* Throws away errors.... *)
  let tap_lines =
    many
      (let* tap_line = tap_line in
       (* Printf.printf "DEBUG: %s%!\n" (show_tap_line tap_line) ; *)
       return tap_line <* end_of_line )
    <?> "Parsing multiple tap lines"

  (* let body = many_till body end_of_input *)
  let read_all (str : string) =
    match Angstrom.parse_string ~consume:All tap_lines str with
    | Ok v ->
        v
    | Error msg ->
        failwith ("TAP Parser failed with:" ^ msg)
end

let print_tap l = l |> show_tap_list |> print_endline

let%expect_test "version" =
  Parser.read_all "TAP Version 14\n" |> print_tap ;
  [%expect {|
    [(Tap.Header "TAP Version 14")] |}]

let%expect_test "plan line" =
  Parser.read_all "1..5\n" |> print_tap ;
  [%expect {|
    [Tap.Plan {count = 5; reason = None}] |}] ;
  Parser.read_all "1..0 # reason\n" |> print_tap ;
  [%expect {|
    [Tap.Plan {count = 0; reason = (Some " reason")}] |}]

let%expect_test "Status line" =
  (* Status line *)
  Parser.read_all "ok\n" |> print_tap ;
  [%expect
    {|
      [Tap.Test {status = Tap.Status.Ok; id = None; description = None;
         comment = None}
        ] |}] ;
  Parser.read_all "not ok\n" |> print_tap ;
  [%expect
    {|
    [Tap.Test {status = Tap.Status.NotOk; id = None; description = None;
       comment = None}
      ] |}] ;
  Parser.read_all "ok 1\n" |> print_tap ;
  Parser.read_all "not ok 1\n" |> print_tap ;
  [%expect
    {|
    [Tap.Test {status = Tap.Status.Ok; id = (Some 1); description = None;
       comment = None}
      ]
    [Tap.Test {status = Tap.Status.NotOk; id = (Some 1); description = None;
       comment = None}
      ] |}]

let%expect_test "status line : title" =
  Parser.read_all "ok 1 - foo\n" |> print_tap ;
  Parser.read_all "not ok 1 - bar\n" |> print_tap ;
  [%expect
    {|
    [Tap.Test {status = Tap.Status.Ok; id = (Some 1); description = (Some "foo");
       comment = None}
      ]
    [Tap.Test {status = Tap.Status.NotOk; id = (Some 1);
       description = (Some "bar"); comment = None}
      ] |}]

let%expect_test "status line : parse description" =
  Parser.read_all "ok 1 - foo\n" |> print_tap ;
  Parser.read_all "not ok 1 - bar\n" |> print_tap ;
  [%expect
    {|
  [Tap.Test {status = Tap.Status.Ok; id = (Some 1); description = (Some "foo");
     comment = None}
    ]
  [Tap.Test {status = Tap.Status.NotOk; id = (Some 1);
     description = (Some "bar"); comment = None}
    ] |}] ;
  Parser.read_all "ok - foo\n" |> print_tap ;
  Parser.read_all "not ok - foo\n" |> print_tap ;
  [%expect
    {|
    [Tap.Test {status = Tap.Status.Ok; id = None; description = (Some "foo");
       comment = None}
      ]
    [Tap.Test {status = Tap.Status.NotOk; id = None; description = (Some "foo");
       comment = None}
      ] |}]

let%expect_test "status line : parse comment" =
  (*
Combinations: 2^3
ok | not ok
with id  | without i
with title | without title
*)
  Parser.read_all "ok 1 - foo #SKIP: lazy engineer\n" |> print_tap ;
  [%expect
    {|
    [Tap.Test {status = Tap.Status.Ok; id = (Some 1);
       description = (Some "foo "); comment = (Some "SKIP: lazy engineer")}
      ] |}] ;
  Parser.read_all "ok 1 #SKIP: missing title\n" |> print_tap ;
  [%expect
    {|
    [Tap.Test {status = Tap.Status.Ok; id = (Some 1); description = None;
       comment = (Some "SKIP: missing title")}
      ] |}] ;
  Parser.read_all "ok #SKIP: missing id+title\n" |> print_tap ;
  [%expect
    {|
  [Tap.Test {status = Tap.Status.Ok; id = None; description = None;
     comment = (Some "SKIP: missing id+title")}
    ] |}]

let%expect_test "multiline" =
  let data = "1..5\nnot ok\nok\nnot ok\nok\nok\n" in
  print_string data ;
  Parser.read_all data |> print_tap ;
  [%expect
    {|
    1..5
    not ok
    ok
    not ok
    ok
    ok
    [Tap.Plan {count = 5; reason = None};
      Tap.Test {status = Tap.Status.NotOk; id = None; description = None;
        comment = None};
      Tap.Test {status = Tap.Status.Ok; id = None; description = None;
        comment = None};
      Tap.Test {status = Tap.Status.NotOk; id = None; description = None;
        comment = None};
      Tap.Test {status = Tap.Status.Ok; id = None; description = None;
        comment = None};
      Tap.Test {status = Tap.Status.Ok; id = None; description = None;
        comment = None}
      ] |}]

let%expect_test "escaping" =
  (* 1) Excaping works for '\\' and '\#'
     2) Only '\\' and '\#' are valid escape sequences *)
  Parser.read_all "ok 1 - hello\\#sharp #TODO: comment\n" |> print_tap ;
  [%expect
    {|
    [Tap.Test {status = Tap.Status.Ok; id = (Some 1);
       description = (Some "hello#sharp "); comment = (Some "TODO: comment")}
      ] |}] ;
  Parser.read_all "ok 1 - hello\\\\sharp #TODO: comment\n" |> print_tap ;
  [%expect
    {|
    [Tap.Test {status = Tap.Status.Ok; id = (Some 1);
       description = (Some "hello\\sharp "); comment = (Some "TODO: comment")}
      ] |}] ;
  (* Illegal escape *)
  let res =
    Angstrom.parse_string ~consume:Angstrom.Consume.All Parser.test_description
      " hello\\Xfoobar"
  in
  print_string (Result.get_error res) ;
  [%expect {|

  parsing test point description: Invalid escaped char 'X' |}] ;
  let res =
    Angstrom.parse_string ~consume:Angstrom.Consume.All Parser.status_line
      "ok 1 - hello\\Xfoobar #foobar"
  in
  Result.get_error res |> print_string;
  [%expect {| parsing test point description: Invalid escaped char 'X' |}]

type tap_line_result = (tap_line,string) result
[@@deriving show]
let%expect_test "escaping tap_line 1" =

  let res =
    Angstrom.parse_string ~consume:Angstrom.Consume.Prefix Parser.tap_line
      "ok 1 - hello\\X #TODO: parser fails"
  in
  Format.printf "%a\n%!" pp_tap_line_result res;
  [%expect {| (Error "Parsing status line > parsing test point description: Invalid escaped char 'X'") |}]


(*
let%expect_test "escaping tap_line 2" =

  (* happy line insert *)
  Parser.read_all "ok 1\nok 2 - hello\\\\world #TODO\n" |> print_tap ;

  [%expect {| parsing test_description: Invalid escaped char 'X' |}] ;
  Parser.read_all "ok 2 - hello\\Xworld #TODO\n" |> print_tap *)
