# betris

A tetris-clone for braille display users.

## How does it work

Every block is a dot.  And the game has been flipped 90 degrees,
so you are playing from right to left instead of top to bottom.

The size of the playing field is identical to the
original game.  Since a braille character is 4 dots
in height, the player needs to scroll up/down to see
the full field of 10 blocks.
There is no need to interact with a screen reader.
Moving the current tetrimino around will also move the
currently visible area.  The tetrimino is always aligned
at the top of the display, except for the last column.

## Demo

This game can be played online.

```console
$ ssh betris@blind.guru
```

