type inline =
    | Text of string
    | Emphatic of string

let print_inline element = 
    match element with
    | Text (text) -> print_endline ("Text: " ^ text)
    | Emphatic (text) -> print_endline ("Emphatic: " ^ text)

let rec print_inlines elements =
    match elements with
    | [] -> ()
    | element::tail -> print_inline element; print_inlines tail

type inline_type =
    | PlainText
    | Emphasise

type search_result =
    | NotFound
    | StartFound of char * int
    | EndFound of inline_type * int
    | Element of inline * int * int

let rec inline_start_index_impl text inlines =
    match inlines with
    | [] -> NotFound
    | char::tail -> let index = String.index_from_opt text 0 char in
        match index with
        | None -> inline_start_index_impl text tail
        | Some (start_index) -> StartFound (char, start_index)

let inline_start_index text =
    let inlines = ['*'] in
        inline_start_index_impl text inlines

exception ParseError of string

let inline_end_index start_char start_index text =
    match start_char with
    | '*' -> let index = String.index_from_opt text (start_index + 1) '*' in
        match index with
        | None -> raise (ParseError ("Failed to find end index for " ^ (String.make 1 start_char) ^ " in " ^ text))
        | Some (end_index) -> EndFound (Emphasise, end_index)

let extract_element start_char start_index text =
    let end_result = inline_end_index start_char start_index text in
        match end_result with
        | EndFound (element_type, end_index) ->
            match element_type with
            | Emphasise -> let element_text = String.sub text (start_index + 1) (end_index - start_index - 1) in
                  Element (Emphatic (element_text), start_index, end_index)
    

let rec convert text =
    let start_index = inline_start_index text in
        match start_index with
        | NotFound -> [Text (text)]
        | StartFound (char, index) -> let element = extract_element char index text in
            match element with
            | Element (inline_element, to_index, from_index) -> 
                let initial_text = Text (String.sub text 0 (to_index - 1)) in
                    let remaining_text = (String.sub text (from_index + 1) (String.length text - (from_index + 1))) in
                        [initial_text; inline_element] @ convert remaining_text


let rec to_elements texts =
    match texts with
    | [] -> []
    | text::tail -> match text with
        | Text (text) -> convert text @ to_elements tail
        | _ -> text::to_elements tail


let s = "Hello *world* woohoo!"
let l = [Text (s)]
let elements = to_elements l
let _ = print_inlines elements
