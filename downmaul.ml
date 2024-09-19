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
    | ListStart of list_type
    | ListEnd of list_type
    | Paragraph of string
    | BlockQuote of string
    | ListItem of string
    | HashHeader of int * string (* <h1>bag</h1> *)
    | FencedCode of char * string * string (* char is either ` or ~ *)

let print_block block =
    match block with
    | Paragraph (text) -> print_endline ("Paragraph: " ^ text)
    | BlankLine -> print_endline "BlankLine"
    | HashHeader (level, text) -> print_endline ("HashHeader of level " ^ (string_of_int level) ^ ": " ^ text);
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

type block_context =
    | CompleteBlock of block
    | IncompleteBlock of block
    | NoContext

let add_to_block line context =
    match context with
    | NoContext -> IncompleteBlock (Paragraph(line))
    | (CompleteBlock (Paragraph (text)) | IncompleteBlock (Paragraph (text))) -> IncompleteBlock (Paragraph(text ^ " " ^ line))
    | _ -> raise ParseError

let line_to_block line context =
    let words = String.split_on_char ' ' line in
        match words with
        | [] -> context
        | first_word::_ -> match first_word with
            | "#" -> CompleteBlock (HashHeader(1, line))
            | "" -> CompleteBlock (BlankLine)
            | _ -> add_to_block line context

let blocks context block =
    match context with
    | NoContext -> [block]
    | (CompleteBlock (prior_block) | IncompleteBlock (prior_block)) -> [prior_block; block]

let rec lines_to_blocks lines context =
    match lines with
        | [] -> (match context with
            | NoContext -> []
            | (CompleteBlock (block) | IncompleteBlock (block)) -> [block])
        | line::t -> let block = line_to_block line context in
            match block with
            | IncompleteBlock (incomplete) -> lines_to_blocks t block
            | CompleteBlock (complete) -> (blocks context complete) @ lines_to_blocks t NoContext
            | NoContext -> raise ParseError

let markdown = Sys.argv.(1)
let lines = read_lines markdown
let blocks = lines_to_blocks lines NoContext
let _ = print_blocks blocks
