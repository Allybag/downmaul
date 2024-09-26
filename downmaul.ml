#use "block.ml"
#use "inline.ml"

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

let rec print_lines lines =
    match lines with
    | [] -> ()
    | blank::tail when (String.length blank == 0) -> print_lines tail
    | line::tail -> print_endline line; print_lines tail

let identity x = x

let text_to_html text = 
    String.concat "" (inlines_to_html (to_elements text))

let markdown = Sys.argv.(1)
let lines = read_lines markdown
let blocks = lines_to_blocks lines NoStream
let html = blocks_to_html blocks text_to_html
let _ = if true then print_lines html else print_blocks blocks
