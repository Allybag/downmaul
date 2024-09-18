type tag = string
type label = string

type inline = (* Vaguely Ordered by precedence *)
    | CodeSpan of int * string
    | Emphasis of int * char * string
    | Link of string * string * string
    | Image of string * string * string
    | Autolink of string
    | RawHtml of string
    | HardLineBreak
    | SoftLineBreak
    | Text of string

type list_type = 
    | Ordered
    | Unordered

type block =
    | ThematicBreak (*  <hr /> *)
    | HashHeader of int * inline (* <h1>bag</h1> *)
    | LineHeader of int * inline (* <h1>bag</h1> *)
    | IndentedCode of inline list
    | FencedCode  of char * string * inline list (* char is either ` or ~ *)
    | HtmlBlock of tag * string
    | LinkReference of label * string * string
    | Paragraph of inline list
    | BlankLine
    | BlockQuote of block list
    | ListItem of block list
    | List of char * list_type * block list
    | Root of block list

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
