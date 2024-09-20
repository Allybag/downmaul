type tag = string
type label = string

exception ParseError
exception NotImplementedError

type inline = (* Vaguely Ordered by precedence *)
    | CodeSpan of int * string
    | Emphasis of int * char * string
    | Link of string * string * string
    | Image of string * string * string
    | Text of string

type list_type =
    | Ordered
    | Unordered

type block =
    | BlankLine
    | ListStart
    | ListEnd
    | ListItem of string
    | BlockQuote of string
    | Paragraph of string
    | CodeBlock of string
    | HashHeader of int * string (* <h1>bag</h1> *)

let print_block block =
    match block with
    | Paragraph (text) -> print_endline ("Paragraph: " ^ text)
    | BlankLine -> print_endline "BlankLine"
    | HashHeader (level, text) -> print_endline ("HashHeader of level " ^ (string_of_int level) ^ ": " ^ text);
    | ListStart -> print_endline ("ListStart")
    | ListEnd -> print_endline ("ListEnd")
    | ListItem (text) -> print_endline ("ListItem: " ^ text);
    | BlockQuote (text) -> print_endline ("BlockQuote: " ^ text);
    | _ -> raise NotImplementedError

let rec print_blocks blocks =
    match blocks with
    | [] -> ()
    | block::t -> print_block block; print_blocks t

let rec read_to_list channel =
    try
        let line = input_line channel in
            line::read_to_list channel
    with
        End_of_file -> []

let read_lines path =
    let in_channel = open_in path in
        let lines = read_to_list in_channel in
            close_in in_channel;
            lines

let rec print_lines l =
    match l with
    | h::t -> print_endline h; print_lines t
    | [] -> ()

type stream_type =
    | List
    | Text
    | Code
    | Quote

type block_stream =
    | BlockStream of block list
    | PendingStream of stream_type * string list
    | NoStream

let is_probably_header string =
    String.starts_with ~prefix:"#" string &&
     String.ends_with ~suffix:"#" string &&
     String.length string <= 6

let to_words string =
    match string with
    | "" -> []
    | non_empty -> String.split_on_char ' ' non_empty

let starts_list_item line =
    String.starts_with ~prefix:("*") line ||
    String.starts_with ~prefix:("+") line ||
    String.starts_with ~prefix:("-") line

let starts_quote line =
    String.starts_with ~prefix:(">") line

let starts_code_block line =
    String.starts_with ~prefix:("```") line ||
    String.starts_with ~prefix:("~~~") line

let ends_code_block line =
    String.ends_with ~suffix:("```") line ||
    String.ends_with ~suffix:("~~~") line

let starts_block line =
    starts_list_item line ||
    starts_quote line ||
    starts_code_block line

let rec lines_to_list lines blocks =
    match lines with
    | [] -> [ListEnd]
    | line::tail -> ListItem (line)::lines_to_list tail blocks

let stream_to_blocks stream =
    match stream with
    | (NoStream | BlockStream (_)) -> raise ParseError
    | PendingStream (stream_type, lines) -> match stream_type with
        | List -> BlockStream (ListStart :: lines_to_list lines [])
        | Text ->  BlockStream [Paragraph(String.concat "\n" lines)]
        | Code ->  BlockStream [CodeBlock("Code")]
        | Quote -> BlockStream [BlockQuote("Quote")]

let ends_stream stream_type line =
    match stream_type with
    | List -> not (starts_list_item line)
    | Quote -> not (starts_quote line)
    | Code -> ends_code_block line
    | Text -> starts_block line || (String.length line == 0)

let add_to_stream line stream =
    match stream with
    | BlockStream (_) -> raise ParseError
    | NoStream -> NoStream
    | PendingStream (stream_type, lines) ->
        if ends_stream stream_type line then stream_to_blocks stream else PendingStream (stream_type, (lines @ [line]))

let line_to_stream line stream =
    let result = add_to_stream line stream in
    match result with
    | BlockStream (_) -> result
    | PendingStream (_) -> result
    | NoStream -> let words = to_words line in
        match words with
        | [] -> BlockStream ([BlankLine])
        | first_word::rest -> match first_word with
            | ("*" | "+" | "-") -> PendingStream (List, ([line]))
            | ">" -> PendingStream (Quote, ([line]))
            | word when is_probably_header word -> BlockStream ([HashHeader(String.length word, (String.concat " " rest))])
            | _ -> PendingStream (Text, ([line]))

let rec lines_to_blocks lines stream =
    match lines with
        | [] -> (match stream with
            | NoStream -> []
            | PendingStream (stream_type, lines) -> []
            | BlockStream (_) -> raise ParseError)
        | line::t -> let result = line_to_stream line stream in
            match result with
            | PendingStream (_) -> lines_to_blocks t result
            | BlockStream (blocks) -> blocks @ (lines_to_blocks t NoStream)
            | NoStream -> raise ParseError

let markdown = Sys.argv.(1)
let lines = read_lines markdown
let blocks = lines_to_blocks lines NoStream
let _ = print_blocks blocks
