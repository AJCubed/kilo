Run `make` and then `./kilo {filename}` to open a file, or `.kilo` to create a new one. 

This is following the tutorial at https://viewsourcecode.org/snaptoken/kilo/03.rawInputAndOutput.html

This has some extended functionalities, such as preserving line indentation, preserving cursor x-position, displaying line numbers, and the option-modifer to shift the cursor to the last/next word.

Remaining todo items:
1. implement highlighting/word selection
2. implement undo/redo
3. fix search so that it isn't one-match-per-line
4. allow moving lines with option-up/down
5. python & markdown syntax highlighting
