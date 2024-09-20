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

The major restriction is that nested block type structures
(lists, block quotes and code blocks) are not allowed,
to allow the parser to be much simpler and less recursive.
