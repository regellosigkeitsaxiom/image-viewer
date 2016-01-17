# image-viewer
Image viewer made for my requirements.

## How to build
You will need ghc, [stack](https://github.com/commercialhaskell/stack.git) and probably cabal

Also your system will need GTK3 and `which` utility.

`stack build` in root directory will build the program.

## How to launch
It does not have any GUI whatsoever and intended to be invoked from command line.
It does not have any options and treats all input as a list of files.
If there is only one file, and it is a directory, it will descend and load all files from it; it is recursive.
If there aren't any, it loads all files from current directory.

## What to do with it
Keybindings are the following:

`space` and `enter` will load next random image

`backspace` will load previous random image

`e` will load next sequential image (they are sorted by name), `w` will load previous.
Note that moving sequentially will not affect random order, so `space, e, e, backspace` will result in the same as `space, backspace`

`p` prints file name to `stdout`, also copies its full path into system buffer

`y` copies full path to file into system buffer, also prints it to `stdout`

`q` and `ESC` exits program.

`0` will return image sequence to the first image. If you want to view all images in sequence, press `0` after the start and then use `e` and `w` to navigate.
