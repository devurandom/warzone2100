#define CURSOR_OFFSET 100
#define MAX_CURSORS 26 

/* must match with IDC_* from resource.h */
#define CURSOR_ARROW                       100  // this is the internal cursor, not from IDC_
#define CURSOR_DEST                        101
#define CURSOR_SIGHT                       102
#define CURSOR_TARGET                      103
#define CURSOR_LARROW                      104
#define CURSOR_RARROW                      105
#define CURSOR_DARROW                      106
#define CURSOR_UARROW                      107
#define CURSOR_DEFAULT                     108
#define CURSOR_EDGEOFMAP                   109
#define CURSOR_ATTACH                      110
#define CURSOR_ATTACK                      111
#define CURSOR_BOMB                        112
#define CURSOR_BRIDGE                      113
#define CURSOR_BUILD                       114
#define CURSOR_EMBARK                      115
#define CURSOR_FIX                         116
#define CURSOR_GUARD                       117
#define CURSOR_JAM                         118
#define CURSOR_LOCKON                      119
#define CURSOR_MENU                        120
#define CURSOR_MOVE                        121
#define CURSOR_NOTPOSSIBLE                 122
#define CURSOR_PICKUP                      123
#define CURSOR_SEEKREPAIR                  124
#define CURSOR_SELECT                      125

/* TODO: do bridge and attach need swapping? */


static const char *cursor_arrow[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "X               ",
  ".X              ",
  "..X             ",
  "...X            ",
  "....X           ",
  ".X.             ",
  "X .X            ",
  "  X.            ",
  "   .X           ",
  "                ",
  "                ",
  "                ",
  "                ",
  "                ",
  "                ",
  "                ",
  "0,0"
};

static const char *cursor_dest[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "                ",
  "                ",
  "                ",
  "     .          ",
  "     .XX        ",
  "     .. X       ",
  "     . X X      ",
  "     . . .X..   ",
  "     .  .XX..   ",
  "    ... .....   ",
  "    ... X.XX.   ",
  "     X ..X..X.  ",
  "        XXXXXX  ",
  "                ",
  "                ",
  "7,9"
};

static const char *cursor_sight[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  " .   .....   .  ",
  "  ..XX   XX..X  ",
  "  ....   ....X  ",
  "  X..X    ..XX  ",
  " .X.X      .X.  ",
  " .X          .X ",
  " .           .X ",
  " .           .X ",
  " .X.       . .X ",
  "  X..     ..XX  ",
  "  ....   ....X  ",
  "  ..XX    X..X  ",
  " .XXX.....XXX.  ",
  "      XXXX      ",
  "                ",
  "7,8"
};

static const char *cursor_target[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "       .        ",
  "      ...       ",
  "    ..X.X..     ",
  "   .X  .   .    ",
  "   .       .X   ",
  "  .X        .   ",
  " ....X .  ....  ",
  "  .X        .   ",
  "  .X        .   ",
  "   .   .   .X   ",
  "    .  .  .X    ",
  "    X.....X     ",
  "      X.X       ",
  "       X        ",
  "                ",
  "7,7"
};

static const char *cursor_larrow[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "                ",
  "                ",
  "                ",
  "        .       ",
  "      ..        ",
  "    ........X   ",
  "  ..........X   ",
  "   X........X   ",
  "     X..XXXXX   ",
  "       X.       ",
  "                ",
  "                ",
  "                ",
  "                ",
  "                ",
  "3,7"
};

static const char *cursor_rarrow[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "                ",
  "                ",
  "                ",
  "                ",
  "       ..       ",
  "   ........     ",
  "   ..........   ",
  "   ..........X  ",
  "   ........X    ",
  "       ..X      ",
  "       X        ",
  "                ",
  "                ",
  "                ",
  "                ",
  "12,8"
};

static const char *cursor_darrow[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "                ",
  "                ",
  "      ...       ",
  "      ...X      ",
  "      ...X      ",
  "      ...XX     ",
  "    .X...X.     ",
  "     .....X     ",
  "     .....      ",
  "      ...X      ",
  "      ...       ",
  "       .X       ",
  "       .        ",
  "                ",
  "                ",
  "7,12"
};

static const char *cursor_uarrow[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "                ",
  "       .        ",
  "       .X       ",
  "      ...       ",
  "      ...X      ",
  "     .....      ",
  "     .....X     ",
  "    . ...X.     ",
  "      ...X      ",
  "      ...X      ",
  "      ...X      ",
  "      XXXX      ",
  "                ",
  "                ",
  "                ",
  "7,4"
};

static const char *cursor_default[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "                ",
  "                ",
  "                ",
  "                ",
  "      X         ",
  "      ..X       ",
  "      ....X     ",
  "       .....X   ",
  "       ..XXXX   ",
  "        .X      ",
  "        .X      ",
  "         X      ",
  "                ",
  "                ",
  "                ",
  "5,6"
};

static const char *cursor_attach[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "                ",
  "                ",
  "                ",
  "                ",
  "                ",
  "  .. ..X .. ..  ",
  "   ... .. ...   ",
  "  .. ..X .. ..  ",
  "                ",
  "                ",
  "                ",
  "                ",
  "                ",
  "                ",
  "                ",
  "8,7"
};

static const char *cursor_attack[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "       .        ",
  "      ...       ",
  "    ..X.X..     ",
  "   .X  .   .    ",
  "   .       .X   ",
  "  .X        .   ",
  " ....X .  ....  ",
  "  .X        .   ",
  "  .X        .   ",
  "   .   .   .X   ",
  "    .  .  .X    ",
  "    X.....X     ",
  "      X.X       ",
  "       X        ",
  "                ",
  "7,7"
};

static const char *cursor_bomb[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "         ..     ",
  "         X.     ",
  "       X ..     ",
  "       ..       ",
  "      XXX.X     ",
  "    .......X    ",
  "   .........X   ",
  "   ..........   ",
  "   ..........   ",
  "   ..........   ",
  "   .........X   ",
  "    .......X    ",
  "      ....X     ",
  "                ",
  "                ",
  "8,8"
};

static const char *cursor_bridge[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "    ..          ",
  "  .....         ",
  " ......       . ",
  " X......    .X  ",
  "  ......  .XX   ",
  " .X......XXX    ",
  " .......XX...   ",
  " ..X...X......  ",
  " .....X.XX....  ",
  "....X.X   ..... ",
  " XX.X     .....X",
  "          ....X ",
  "          X...X ",
  "            XX  ",
  "                ",
  "8,8"
};

static const char *cursor_build[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "                ",
  "                ",
  "                ",
  "       ..X.X    ",
  "     X...       ",
  "     ...X       ",
  "    ...XX       ",
  "   ...X..X      ",
  "    .X  ..X     ",
  "         ..X    ",
  "          ..    ",
  "           X    ",
  "                ",
  "                ",
  "                ",
  "7,7"
};

static const char *cursor_embark[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "                ",
  "     ......     ",
  "     ......     ",
  "    ........X   ",
  "     ......X    ",
  "     X....X     ",
  "    ..X..X..    ",
  "     ..XX..     ",
  "      ....      ",
  "       ..       ",
  "                ",
  "                ",
  "                ",
  "                ",
  "                ",
  "8,11"
};

static const char *cursor_fix[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "                ",
  "                ",
  "      .X        ",
  "       .        ",
  "   .   .        ",
  "   .XX..        ",
  "    .....       ",
  "       ...XX    ",
  "        .....   ",
  "         .   X  ",
  "         .      ",
  "         .XX    ",
  "                ",
  "                ",
  "                ",
  "8,8"
};

static const char *cursor_guard[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "                ",
  "                ",
  "                ",
  "   ...   ...    ",
  "   ..XXXXX..    ",
  "   .........    ",
  "   .......XX    ",
  "    X.....XX    ",
  "    .......     ",
  "     X...XX     ",
  "      .X.X      ",
  "       .X       ",
  "                ",
  "                ",
  "                ",
  "8,8"
};

static const char *cursor_jam[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "                ",
  "                ",
  "     ..X..      ",
  "    .X   X.     ",
  "    . . . .X    ",
  "    X X. X X    ",
  "    . X.X .X    ",
  "    .X... .     ",
  "     X...X      ",
  "     .....      ",
  "      XXXX      ",
  "                ",
  "                ",
  "                ",
  "                ",
  "7,7"
};

static const char *cursor_lockon[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "                ",
  "                ",
  "     ..X..      ",
  "    .X   X.     ",
  "    . . . .X    ",
  "    X X. X X    ",
  "    . X.X .X    ",
  "    .X... .     ",
  "     X...X      ",
  "     .....      ",
  "      XXXX      ",
  "                ",
  "                ",
  "                ",
  "                ",
  "7,7"
};

static const char *cursor_menu[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "                ",
  "                ",
  "                ",
  "                ",
  "     .X         ",
  "     ...X       ",
  "      ....X     ",
  "      ....X     ",
  "       .X       ",
  "       .X       ",
  "                ",
  "                ",
  "                ",
  "                ",
  "                ",
  "5,5"
};

static const char *cursor_move[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "                ",
  "                ",
  "     . ..X.     ",
  "    ..X  X..    ",
  "   ....  ....   ",
  "    X..  ..XX   ",
  "   .        .   ",
  "   .        .   ",
  "    X..  ..XX   ",
  "   ....  ....   ",
  "    ..X  X..    ",
  "     . ..X.     ",
  "                ",
  "                ",
  "                ",
  "7,7"
};

static const char *cursor_notpossible[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "                ",
  "                ",
  "    X      X    ",
  "   ..X    ..X   ",
  "   ...X  ...X   ",
  "    ...X...X    ",
  "     .....X     ",
  "      ...X      ",
  "     .....X     ",
  "    ...X...X    ",
  "   ...X  ...X   ",
  "   ..X    ..X   ",
  "    X      X    ",
  "                ",
  "                ",
  "7,8"
};

static const char *cursor_pickup[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "      . .       ",
  "      ...       ",
  "       .        ",
  "      ...       ",
  "      ...       ",
  "       .X       ",
  "      ...X      ",
  "     .....X     ",
  "    ..XXX..     ",
  "    .     .     ",
  "    .     .     ",
  "    .X    .     ",
  "     .X  .      ",
  "                ",
  "                ",
  "7,10"
};

static const char *cursor_seekrepair[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  "                ",
  "                ",
  "    .           ",
  "    .X      .   ",
  " X  .X    ....  ",
  " .XX.X    ..... ",
  "  ....X   ....  ",
  "     ..XXX  .   ",
  "      ....X     ",
  "      .X  .     ",
  "      .X        ",
  "       .X       ",
  "                ",
  "                ",
  "                ",
  "8,7"
};

static const char *cursor_select[] = {
  /* width height num_colors chars_per_pixel */
  "    16    16        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                ",
  " ....      .... ",
  " .XXX      XXX. ",
  " .X          X. ",
  " .X          X. ",
  "                ",
  "                ",
  "       .        ",
  "                ",
  "                ",
  "                ",
  " .X          X. ",
  " .X          X. ",
  " .XXX      XXX. ",
  " ....      .... ",
  "                ",
  "7,7"
};

static SDL_Cursor *init_system_cursor(const char *image[])
{
  int i, row, col;
  Uint8 data[4*16];
  Uint8 mask[4*16];
  int hot_x, hot_y;

  i = -1;
  for ( row=0; row<16; ++row ) {
    for ( col=0; col<16; ++col ) {
      if ( col % 8 ) {
        data[i] <<= 1;
        mask[i] <<= 1;
      } else {
        ++i;
        data[i] = mask[i] = 0;
      }
      switch (image[4+row][col]) {
        case 'X':
          data[i] |= 0x01;
          mask[i] |= 0x01;
          break;
        case '.':
          mask[i] |= 0x01;
          break;
        case ' ':
          break;
      }
    }
  }
  sscanf(image[4+row], "%d,%d", &hot_x, &hot_y);
  return SDL_CreateCursor(data, mask, 16, 16, hot_x, hot_y);
}

