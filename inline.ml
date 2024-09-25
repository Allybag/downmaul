type inline =
    | Text of string
    | Emphatic of string
    | Strong of string

let print_inline element =
    match element with
    | Text (text) -> print_endline ("Text: " ^ text)
    | Emphatic (text) -> print_endline ("Emphatic: " ^ text)
    | Strong (text) -> print_endline ("Strong: " ^ text)

let rec print_inlines elements =
    match elements with
    | [] -> ()
    | element::tail -> print_inline element; print_inlines tail

type inline_type =
    | PlainText
    | Emphasise
    | Strengthen

type search_result =
    | NotFound
    | StartFound of char * int
    | EndFound of inline_type * int
    | Element of inline * int * int

let rec inline_start_index_impl line index inlines =
    match inlines with
    | [] -> NotFound
    | char::tail -> let inline_index = String.index_from_opt line index char in
        match inline_index with
        | None -> inline_start_index_impl line index tail
        | Some (start_index) -> (assert (start_index >= index));StartFound (char, start_index)

let inline_start_index line index =
    let inlines = ['*'] in
        inline_start_index_impl line index inlines

exception ParseError of string

let inc opt =
    match opt with
    | None -> opt
    | Some (num) -> Some (num + 1)

let blah start_char start_index line =
    match start_char with
    |'*' when (line.[start_index + 1] == '*') -> inc (String.index_from_opt line (start_index + 2) '*')
    |'*' -> String.index_from_opt line (start_index + 1) '*'

let inline_length start_char start_index line =
    match start_char with
    | '*' -> let result = blah start_char start_index line in
        match result with
        | None -> raise (ParseError ("Failed to find length for " ^ (String.make 1 start_char) ^ " from " ^ (string_of_int start_index) ^ " in " ^ line))
        | Some (end_index) when (line.[end_index - 1] == '*') -> EndFound (Strengthen, end_index - start_index)
        | Some (end_index) -> EndFound (Emphasise, end_index - start_index)

let sub line start_index length where : string =
    print_endline ("Sub: " ^ where ^ ", start: " ^ (string_of_int start_index) ^ ", length: " ^ (string_of_int length) ^ ", total: " ^ (string_of_int (String.length line)));
    String.sub line start_index length

let extract_element start_char start_index line =
    let end_result = inline_length start_char start_index line in
        match end_result with
        | EndFound (element_type, length) ->
            match element_type with
            | Emphasise ->
                let element_text = sub line (start_index + 1) (length - 1) "extract_element" in
                  Element (Emphatic (element_text), start_index, length)
            | Strengthen ->
                let element_text = sub line (start_index + 2) (length - 3) "extract_element" in
                  Element (Strong (element_text), start_index, length)

let rec line_to_elements line index =
    let start_result = inline_start_index line index in
        match start_result with
        | NotFound -> [Text (String.sub line index ((String.length line) - index))]
        | StartFound (char, start_index) -> let element = extract_element char start_index line in
            match element with
            | Element (inline_element, from_index, length) ->
                let initial_text = Text (sub line index (from_index - index) "line_to_elements") in
                        [initial_text; inline_element] @ line_to_elements line (from_index + length + 1)

let to_elements line =
    line_to_elements line 0

let s = "Hello|*world*|**bag**|woohoo!"
let elements = to_elements s
let _ = print_inlines elements
