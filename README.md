# DownMaul

A simple, heavily restricted MarkDown to HTML translator.

The following subset of MarkDown will be implemented:
* Lists
* Block Quotes
* Code Blocks (Not indented, just fenced)
* Paragraphs
* ATX or hash style headers
* Links
* Images
* Strong or emphatic text
* Inline code spans
* Line breaks

The major restriction is that block type structures
(lists, block quotes and code blocks) or inline structures
(links, emphasised text and inline code) cannot be nested.
Blocks can contain multiple inline elements in each block,
but blocks cannot contain blocks and inline elements cannot
contain other inline elements.

If this rule is violiated, you will probably get a parser
error, but could easily generate invalid or wrong html instead.
