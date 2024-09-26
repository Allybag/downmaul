type inline =
    | Text of string
    | Emphatic of string
    | Strong of string
    | Code of string
    | Link of string
    | Image of string

let print_inline element =
    match element with
    | Text (text) -> print_endline ("Text: " ^ text)
    | Emphatic (text) -> print_endline ("Emphatic: " ^ text)
    | Strong (text) -> print_endline ("Strong: " ^ text)
    | Code (text) -> print_endline ("Code: " ^ text)
    | Link (text) -> print_endline ("Link: " ^ text)
    | Image (text) -> print_endline ("Image: " ^ text)

let rec print_inlines elements =
    match elements with
    | [] -> ()
    | element::tail -> print_inline element; print_inlines tail

type inline_type =
    | PlainText
    | Emphasise
    | Strengthen
    | Source
    | LinkReference
    | ImageReference

let rec inline_start_index_impl line index inlines =
    match inlines with
    | [] -> (' ', index)
    | char::tail -> let inline_index = String.index_from_opt line index char in
        match inline_index with
        | None -> inline_start_index_impl line index tail
        | Some (start_index) -> (assert (start_index >= index)); (char, start_index)

let inline_start_index line index =
    let inlines = ['*'; '`'; '!'; '['] in (* Dubiously '!' must be before '[' *)
        inline_start_index_impl line index inlines

exception ParseError of string
exception NotImplementedError

let inc opt =
    match opt with
    | None -> opt
    | Some (num) -> Some (num + 1)

let link_end_index start_index line =
    let text_end = String.index_from_opt line (start_index + 1) ']' in
        match text_end with
        | Some (text_end_index) when line.[text_end_index + 1] == '[' -> String.index_from_opt line (text_end_index + 2) ']'
        | Some (text_end_index) when line.[text_end_index + 1] == '(' -> String.index_from_opt line (text_end_index + 2) ')'
        | _ -> None

let inline_end_index start_char start_index line =
    match start_char with
    |'*' when (line.[start_index + 1] == '*') -> inc (String.index_from_opt line (start_index + 2) '*')
    |'*' -> String.index_from_opt line (start_index + 1) '*'
    |'!' when (line.[start_index + 1] == '[') -> link_end_index start_index line
    |'[' -> link_end_index start_index line
    |'`' -> String.index_from_opt line (start_index + 1) '`'
    | _ -> raise NotImplementedError

let inline_length start_char start_index line =
    let result = inline_end_index start_char start_index line in
        match result with
        | None -> raise (ParseError ("Failed to find length for " ^ (String.make 1 start_char) ^ " from " ^ (string_of_int start_index) ^ " in " ^ line))
        | Some (end_index) when (start_char == '*' && line.[end_index - 1] == '*') -> (Strengthen, end_index - start_index)
        | Some (end_index) when (start_char == '*') -> (Emphasise, end_index - start_index)
        | Some (end_index) when (start_char == '`') -> (Source, end_index - start_index)
        | Some (end_index) when (start_char == '[') -> (LinkReference, end_index - start_index)
        | Some (end_index) when (start_char == '!') -> (ImageReference, end_index - start_index)
        | _ -> raise NotImplementedError

let sub line start_index length where : string =
    print_endline ("Sub: " ^ where ^ ", start: " ^ (string_of_int start_index) ^ ", length: " ^ (string_of_int length) ^ ", total: " ^ (string_of_int (String.length line)));
    String.sub line start_index length

let extract_element start_char start_index line =
    let element_type, length = inline_length start_char start_index line in
        match element_type with
        | PlainText -> raise (ParseError "Attempting to extract a plain text element")
        | Emphasise ->
            let element_text = sub line (start_index + 1) (length - 1) "extract_element" in
              (Emphatic (element_text), length)
        | Strengthen ->
            let element_text = sub line (start_index + 2) (length - 3) "extract_element" in
              (Strong (element_text), length)
        | Source ->
            let element_text = sub line (start_index + 1) (length - 1) "extract_element" in
              (Code (element_text), length)
        | LinkReference ->
            let element_text = sub line (start_index + 1) (length - 1) "extract_element" in
              (Link (element_text), length)
        | ImageReference ->
            let element_text = sub line (start_index + 1) (length - 1) "extract_element" in
              (Image (element_text), length)

let rec line_to_elements line index =
    let start_char, start_index = inline_start_index line index in
        match start_char with
        | ' ' -> [Text (String.sub line index ((String.length line) - index))]
        | _ -> let element, length  = extract_element start_char start_index line in
                let initial_text = Text (sub line index (start_index - index) "line_to_elements") in
                        [initial_text; element] @ line_to_elements line (start_index + length + 1)

let to_elements line =
    line_to_elements line 0

let s = "![dog][dogimage]Hello|*foo*|**bag**|woohoo|`let x = false`|[here](example.org)|world"
let elements = to_elements s
let _ = print_inlines elements
