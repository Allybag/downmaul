type tag = string
type label = string

type inline = (* Vaguely Ordered by precedence *)
    | CodeSpan of int * string
    | Emphasis of int * char * string
    | Link of string * string * string
    | Image of string * string * string
    | LineBreak
    | Text of string

type list_type = 
    | Ordered
    | Unordered

type block =
    | Text of string
    | ThematicBreak (*  <hr /> *)
    | BlankLine
    | ListStart of list_type
    | ListEnd of list_type
    | HashHeader of int * string (* <h1>bag</h1> *)
    | Paragraph of inline list
    | BlockQuote of block list
    | ListItem of block list
    | IndentedCode of inline list
    | FencedCode  of char * string * inline list (* char is either ` or ~ *)
    | Empty

let markdown = Sys.argv.(1)
let _ = print_endline markdown

let lines = []

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
        h::t -> print_endline h; print_lines t
        | [] -> ()

let lines = read_lines markdown
let _ = print_lines lines

let line_to_block line last_block =
    let words = String.split_on_char ' ' line in
        match words with
            [] -> last_block, Empty
            | h::t -> match h with
                 "#" -> print_endline "Hash, woohoo!"; (last_block, (HashHeader(1, line)))
                |"" -> print_endline "Blank line"; (last_block, (BlankLine))
                |_ -> match last_block with
                    Text (text) -> (Empty, Text(String.concat line [text]))
                    |_ -> (last_block, Text(line))
    
let rec lines_to_blocks lines last_block =
    match lines with
        [] -> (match last_block with
            Empty -> []
            |_ -> [last_block])
        | h::t -> let left, right = line_to_block h last_block in
            match left with
            Empty -> lines_to_blocks t right
            |_ -> left :: lines_to_blocks t right

let blocks = lines_to_blocks lines Empty
let _ = print_int (List.length blocks)
