type status = Ok | NotOk [@@deriving show]

type tap_line =
  | Header of string
  | Plan of {count: int; reason: string option}
  | Test of
      { status: status
      ; id: int option
      ; description: string option
      ; comment: string option }
  | Verbatim of string (* Ideally fold verbatim into one blob *)
  | Bail of string
[@@deriving show]

type tap = tap_line list [@@deriving show]

module Parser = struct
  open Angstrom

  module S = struct
    type t = [`Unescaped | `Escaped]

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
          None

    let text terminal =
      let buf = Buffer.create 64 in
      let* (matched, (state,buf)) : string * (t * Buffer.t) =
        scan (`Unescaped, buf) (fun (state, buf) c ->
            match (state, c) with
            | `Unescaped, c ->
                unescaped terminal buf c
            | `Escaped, c ->
                escaped terminal buf c )
      in
      let buf_str = Buffer.to_bytes buf |> String.of_bytes in
      Buffer.clear buf;
      match state,(String.trim matched) with
      | (`Unescaped,"") ->
          return None
      | `Unescaped, _ ->
          return  @@ Some buf_str
      | `Escaped, matched ->
          (* Invalid state when ending parsing *)
          let msg =
            let escape_char = String.get buf_str (String.length buf_str-1) in
            Printf.sprintf "invalid escape char, %c, in string %s%c" escape_char matched escape_char
          in
           fail msg
  end

  let is_digit = function '0' .. '9' -> true | _ -> false

  let digits = take_while1 is_digit >>| int_of_string

  let whitespace = skip_while (fun c -> c = ' ' || c = '\t')

  let tap_header = string "TAP Version 14"

  let tap_ok = string "ok" *> return Ok

  let tap_notok = string "not ok" *> return NotOk

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

  let escaped_chars =
    choice
      [ (let+ _ = string "\\#" in
         '#' )
      ; (let+ _ = string "\\\\" in
         '\\' )
         ]


  let __test_description =
    (* - Detect prefix it can be one or more spaces or ' - '
       - Read the string until # or \n
       - If empty => None.
       - Must propagate failure.
    *)

    (* prefix can be a space or nothing *)
    let* () = skip_while (fun c -> c == ' ' || c == '-') in
    let+ comment = many @@ choice [escaped_chars; satisfy description_end] in
    match List.is_empty comment with
    | true ->
        None
    | false ->
        comment |> List.to_seq |> String.of_seq |> Option.some

  let test_description =
    let* _ = skip_while (fun c -> c == ' ' || c == '-') in
     S.text description_end <?> "parsing test_description"

  let test_directive = whitespace *> comment_to_eol

  let status_line =
    let id = opt @@ (char ' ' *> digits) in
    let+ status = status
    and+ id = id
    and+ description = test_description
    and+ comment = opt test_directive in
    (* return (status, id ,description,comment) *)
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
        plan
    | Some 'o' | Some 'n' ->
        status_line <?> "Parsing status line"
    | Some other ->
        fail (Printf.sprintf "Unknown char at pos ? %c" other)
    | None ->
        fail "could not parse tap_line "

  let tap =
    many (tap_line <?> "Parsing tap line" <* end_of_line)
    <?> "Pasing multiple tap lines"

  (* let body = many_till body end_of_input *)
  let read_all (str : string) =
    match Angstrom.parse_string ~consume:All tap str with
    | Ok v ->
        v
    | Error msg ->
        failwith msg
end

let print_tap l = l |> show_tap |> print_endline

let%expect_test "version" =
  Parser.read_all "TAP Version 14\n" |> print_tap ;
  [%expect {| [(Tap.Header "TAP Version 14")] |}]

let%expect_test "plan line" =
  Parser.read_all "1..5\n" |> print_tap ;
  [%expect {| [Tap.Plan {count = 5; reason = None}] |}] ;
  Parser.read_all "1..0 # reason\n" |> print_tap ;
  [%expect {| [Tap.Plan {count = 0; reason = (Some " reason")}] |}]

let%expect_test "Status line" =
  (* Status line *)
  Parser.read_all "ok\n" |> print_tap ;
  [%expect
    {| [Tap.Test {status = Tap.Ok; id = None; description = None; comment = None}] |}] ;
  Parser.read_all "not ok\n" |> print_tap ;
  [%expect
    {|
    [Tap.Test {status = Tap.NotOk; id = None; description = None; comment = None}
      ] |}] ;
  Parser.read_all "ok 1\n" |> print_tap ;
  Parser.read_all "not ok 1\n" |> print_tap ;
  [%expect
    {|
    [Tap.Test {status = Tap.Ok; id = (Some 1); description = None; comment = None}
      ]
    [Tap.Test {status = Tap.NotOk; id = (Some 1); description = None;
       comment = None}
      ] |}]

let%expect_test "status line : title" =
  Parser.read_all "ok 1 - foo\n" |> print_tap ;
  Parser.read_all "not ok 1 - bar\n" |> print_tap ;
  [%expect
    {|
    [Tap.Test {status = Tap.Ok; id = (Some 1); description = (Some "foo");
       comment = None}
      ]
    [Tap.Test {status = Tap.NotOk; id = (Some 1); description = (Some "bar");
       comment = None}
      ] |}]

let%expect_test "status line : parse description" =
  Parser.read_all "ok 1 - foo\n" |> print_tap ;
  Parser.read_all "not ok 1 - bar\n" |> print_tap ;
  [%expect
    {|
  [Tap.Test {status = Tap.Ok; id = (Some 1); description = (Some "foo");
     comment = None}
    ]
  [Tap.Test {status = Tap.NotOk; id = (Some 1); description = (Some "bar");
     comment = None}
    ] |}] ;
  Parser.read_all "ok - foo\n" |> print_tap ;
  Parser.read_all "not ok - foo\n" |> print_tap ;
  [%expect
    {|
    [Tap.Test {status = Tap.Ok; id = None; description = (Some "foo");
       comment = None}
      ]
    [Tap.Test {status = Tap.NotOk; id = None; description = (Some "foo");
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
    [Tap.Test {status = Tap.Ok; id = (Some 1); description = (Some "foo ");
       comment = (Some "SKIP: lazy engineer")}
      ] |}] ;
  Parser.read_all "ok 1 #SKIP: missing title\n" |> print_tap ;
  [%expect
    {|
    [Tap.Test {status = Tap.Ok; id = (Some 1); description = None;
       comment = (Some "SKIP: missing title")}
      ] |}] ;
  Parser.read_all "ok #SKIP: missing id+title\n" |> print_tap ;
  [%expect
    {|
  [Tap.Test {status = Tap.Ok; id = None; description = None;
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
      Tap.Test {status = Tap.NotOk; id = None; description = None; comment = None};
      Tap.Test {status = Tap.Ok; id = None; description = None; comment = None};
      Tap.Test {status = Tap.NotOk; id = None; description = None; comment = None};
      Tap.Test {status = Tap.Ok; id = None; description = None; comment = None};
      Tap.Test {status = Tap.Ok; id = None; description = None; comment = None}] |}]

let%expect_test "escaping" =
  (* 1) Excaping works for '\\' and '\#'
     2) Only '\\' and '\#' are valid escape sequences *)
  Parser.read_all "ok 1 - hello\\#sharp #TODO: comment\n" |> print_tap ;
  [%expect
    {|
    [Tap.Test {status = Tap.Ok; id = (Some 1);
       description = (Some "hello#sharp "); comment = (Some "TODO: comment")}
      ] |}] ;
  Parser.read_all "ok 1 - hello\\\\sharp #TODO: comment\n" |> print_tap ;
  [%expect
    {|
    [Tap.Test {status = Tap.Ok; id = (Some 1);
       description = (Some "hello\\sharp "); comment = (Some "TODO: comment")}
      ] |}] ;
  (* Illegal escape *)
  match
    Angstrom.parse_string ~consume:Angstrom.Consume.Prefix Parser.test_description
      " hello\\xfoobar"
  with
  | Ok res ->
      failwith @@ "ERROR unexpected success:"  ^ (Option.get res)
  | Error err ->
      print_endline err;
  [%expect {| parsing test_description: invalid escape char, x, in string hello\x |}]

(* | exception Failure msg ->
    print_endline msg ; [%expect {| |}] *)
