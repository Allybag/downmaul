exception ParseError of string
exception NotImplementedError

let escape_code char =
    match char with
    |'&' -> "&amp;"
    |'<' -> "&lt;"
    |'>' -> "&gt;"
    |_ -> raise NotImplementedError

let rec escape_char char text start_index =
    let opt_index = String.index_from_opt text start_index char in
    match opt_index with
    | None -> text
    | Some (index) -> let code = escape_code char in
        let escaped = String.concat "" [String.sub text 0 index; code; String.sub text (index + 1) ((String.length text) - index - 1)] in
            escape_char char escaped (index + (String.length code))

let rec escape_chars_impl chars text =
    match chars with
    |[] -> text
    |char::tail -> escape_chars_impl tail (escape_char char text 0)

let escape_chars text =
    let chars = ['&'; '<'; '>'] in
        escape_chars_impl chars text

