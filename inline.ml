#use "common.ml"

type inline =
    | Text of string
    | Emphatic of string
    | Strong of string
    | Code of string
    | Link of string * string
    | Image of string * string

let print_inline element =
    match element with
    | Text (text) -> print_endline ("Text: " ^ text)
    | Emphatic (text) -> print_endline ("Emphatic: " ^ text)
    | Strong (text) -> print_endline ("Strong: " ^ text)
    | Code (text) -> print_endline ("Code: " ^ text)
    | Link (text, _) -> print_endline ("Link: " ^ text)
    | Image (text, _) -> print_endline ("Image: " ^ text)

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

let valid_start start_index char line =
    match char with
    |'!' -> (start_index + 1 < String.length line) && line.[start_index + 1] == '['
    |_ -> true

let rec inline_start_index_impl line index inlines first_char first_index =
    match inlines with
    | [] -> (first_char, first_index)
    | char::tail -> let inline_index = String.index_from_opt line index char in
        match inline_index with
        | Some (start_index) when (valid_start start_index char line) && (first_char == ' ' || start_index < first_index) -> inline_start_index_impl line index tail char start_index
        | _ -> inline_start_index_impl line index tail first_char first_index

let inline_start_index line index =
    let inlines = ['*'; '`'; '!'; '['] in
        inline_start_index_impl line index inlines ' ' 0

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

let extract_link line start_index =
    let text_end = String.index_from line start_index ']' in
        match line.[text_end + 1] with
        |'(' -> let source_end = String.index_from line text_end ')' in
            ((String.sub line start_index (text_end - start_index)), (String.sub line (text_end + 2) (source_end - text_end - 2)))
        |('[' | _) -> raise NotImplementedError
    

let extract_element start_char start_index line =
    let element_type, length = inline_length start_char start_index line in
        match element_type with
        | PlainText -> raise (ParseError "Attempting to extract a plain text element")
        | Emphasise ->
            let element_text = String.sub line (start_index + 1) (length - 1) in
              (Emphatic (element_text), length)
        | Strengthen ->
            let element_text = String.sub line (start_index + 2) (length - 3) in
              (Strong (element_text), length)
        | Source ->
            let element_text = String.sub line (start_index + 1) (length - 1) in
              (Code (element_text), length)
        | LinkReference -> let element_text, element_source = extract_link line (start_index + 1) in
              (Link (element_text, element_source), length)
        | ImageReference -> let element_text, element_source = extract_link line (start_index + 2) in
              (Image (element_text, element_source), length)

let rec line_to_elements line index =
    let start_char, start_index = inline_start_index line index in
        match start_char with
        | ' ' -> [Text (String.sub line index ((String.length line) - index))]
        | _ -> let element, length  = extract_element start_char start_index line in
                let initial_text = Text (String.sub line index (start_index - index)) in
                        [initial_text; element] @ line_to_elements line (start_index + length + 1)

let to_elements line =
    line_to_elements line 0

let inline_to_html inline =
    match inline with
    | Text (text) -> (escape_chars text)
    | Emphatic (text) -> "<em>" ^ (escape_chars text) ^ "</em>"
    | Strong (text) -> "<strong>" ^ (escape_chars text) ^ "</strong>"
    | Code (text) -> "<code>" ^ (escape_chars text) ^ "</code>"
    | Link (text, source) -> "<a href=\"" ^ (escape_chars source) ^ "\">" ^ (escape_chars text) ^ "</a>"
    | Image (text, source) -> "<img src=\"" ^ (escape_chars source) ^ "\">" ^ (escape_chars text) ^ "</img>"

let rec inlines_to_html inlines =
    match inlines with
        | [] -> []
        | inline::tail -> inline_to_html inline :: inlines_to_html tail

