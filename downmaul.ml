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

type block_context =
    | CompleteBlock of block
    | IncompleteBlock of block
    | NoContext

let add_to_block line context =
    match context with
    | NoContext -> IncompleteBlock (Paragraph(line))
    | IncompleteBlock (Paragraph (text)) -> IncompleteBlock (Paragraph(text ^ "\n" ^ line))
    | IncompleteBlock (CodeBlock (text)) -> IncompleteBlock (CodeBlock(text ^ "\n" ^ line))
    | _ -> raise ParseError

let is_probably_header string =
    String.starts_with ~prefix:"#" string &&
     String.ends_with ~suffix:"#" string &&
     String.length string <= 6

let to_words string =
    match string with
    | "" -> []
    | non_empty -> String.split_on_char ' ' non_empty

let line_to_block line context =
    let words = to_words line in
        match words with
        | [] -> CompleteBlock (BlankLine)
        | first_word::rest -> match first_word with
            | ("*" | "+" | "-") -> CompleteBlock (ListItem(String.concat " " rest))
            | ">" -> CompleteBlock (BlockQuote(String.concat " " rest))
            | word when is_probably_header word -> CompleteBlock (HashHeader(String.length word, (String.concat " " rest)))
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
