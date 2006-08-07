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
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "X                               ",
  "XX                              ",
  "X.X                             ",
  "X..X                            ",
  "X...X                           ",
  "X....X                          ",
  "X.....X                         ",
  "X......X                        ",
  "X.......X                       ",
  "X........X                      ",
  "X.....XXXXX                     ",
  "X..X..X                         ",
  "X.X X..X                        ",
  "XX  X..X                        ",
  "X    X..X                       ",
  "     X..X                       ",
  "      X..X                      ",
  "      X..X                      ",
  "       XX                       ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "0,0"
};

static const char *cursor_dest[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "           ..                   ",
  "           ...X                 ",
  "           ..X.X                ",
  "           ..X .X               ",
  "           .X.X .X              ",
  "           .X.X  .X             ",
  "           .X .X  .X            ",
  "           .X .X   .......      ",
  "           .X  .X  .XX....X     ",
  "           .X  .X .XXX....X     ",
  "           .X   ..XXXX....X     ",
  "          ...X  ..XXXX....X     ",
  "         .....X ..........X     ",
  "         .....X ..........X     ",
  "         .....X .XX.XX.XX..     ",
  "          ...X .X..X..X..X..    ",
  "           XX  .X..X..X..XX.X   ",
  "               X...........XX   ",
  "                XXXXXXXXXXXX    ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "15,18"
};

static const char *cursor_sight[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "            .......             ",
  "   ..     ...........     ..    ",
  "   ....  X..XXXXXXX..X  ....X   ",
  "    .....XXXX      XXX.....XX   ",
  "    .......X        .......X    ",
  "     ........     ........XX    ",
  "     .......XX     .......X     ",
  "    XX.....XX       .....XXX    ",
  "   ..X....XX         ....X..X   ",
  "   ..XX..XX           ..XX..X   ",
  "  ...X .XX             .X ...X  ",
  "  ..XX  X               X  ..X  ",
  "  ..X                      ..X  ",
  "  ..X                      ..X  ",
  "  ..X                      ..X  ",
  "  ..X                      ..X  ",
  "  ...  .               .X ...X  ",
  "   ..X ..             ..X ..XX  ",
  "   ..X....           ....X..X   ",
  "    XX.....         .....XXXX   ",
  "     .......       .......XX    ",
  "     ........     ........XX    ",
  "    .......XXX    XX.......X    ",
  "    .....XXX        XX.....X    ",
  "   ....XXX...     ...XXX....    ",
  "   ..XXX X...........XXX X..X   ",
  "    XX    XX.......XXX     XX   ",
  "            XXXXXXXX            ",
  "                                ",
  "                                ",
  "15,16"
};

static const char *cursor_target[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "               .                ",
  "               .X               ",
  "            ....X..             ",
  "          ......X....           ",
  "         ...XXX.XXX...          ",
  "        ..XX   .X   X..         ",
  "       ..X     .X     ..        ",
  "      ..X      XX      ..       ",
  "      ..X              ..X      ",
  "     ..X                ..X     ",
  "     ..X                ..X     ",
  "     ..X                ..X     ",
  "   ........X   .    ........X   ",
  "    XXXXXXXX   X     XXXXXXXX   ",
  "     ..X                ..X     ",
  "     ..X                ..X     ",
  "     ..X                ..X     ",
  "      ..                .X      ",
  "      ..X      .       ..X      ",
  "       ..      .X     ..X       ",
  "        ..     .X    ..X        ",
  "         ...   .X   ..X         ",
  "         X......X....X          ",
  "          XX....X..XX           ",
  "            XXX.XXX             ",
  "               .X               ",
  "               XX               ",
  "                                ",
  "                                ",
  "15,15"
};

static const char *cursor_larrow[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                ..              ",
  "              ...X              ",
  "            ....X               ",
  "          ...............       ",
  "        .................X      ",
  "      ...................X      ",
  "    .....................X      ",
  "    XX...................X      ",
  "      XX.................X      ",
  "        XX...............X      ",
  "          XX....XXXXXXXXXX      ",
  "            XX...X              ",
  "              XX..X             ",
  "                XXX             ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "7,15"
};

static const char *cursor_rarrow[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "              ..                ",
  "               ...              ",
  "                ....            ",
  "       ...............          ",
  "       .................        ",
  "       ...................      ",
  "       .....................X   ",
  "       ...................XX    ",
  "       .................XX      ",
  "       ...............XX        ",
  "        XXXXXXXX....XX          ",
  "              X...XX            ",
  "              ..XX              ",
  "              XX                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "25,16"
};

static const char *cursor_darrow[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "            .......             ",
  "            .......X            ",
  "            .......X            ",
  "            .......X            ",
  "            .......X            ",
  "            .......X            ",
  "            .......X XX         ",
  "         .X .......XX.X         ",
  "         ..X.......X..X         ",
  "          ...........X          ",
  "          ...........X          ",
  "           .........X           ",
  "           .........X           ",
  "            .......X            ",
  "            .......X            ",
  "             .....X             ",
  "             .....X             ",
  "              ...X              ",
  "              ...X              ",
  "               .X               ",
  "               .X               ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "15,24"
};

static const char *cursor_uarrow[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "               .                ",
  "               .X               ",
  "              ...X              ",
  "              ...X              ",
  "             .....X             ",
  "             .....X             ",
  "            .......X            ",
  "            .......X            ",
  "           .........X           ",
  "           .........X           ",
  "          ...........X          ",
  "          ...........X          ",
  "         ..X.......X..X         ",
  "         .X .......XX.X         ",
  "            .......X XX         ",
  "            .......X            ",
  "            .......X            ",
  "            .......X            ",
  "            .......X            ",
  "            .......X            ",
  "            .......X            ",
  "             XXXXXXX            ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "15,8"
};

static const char *cursor_default[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "            ..XX                ",
  "            ....XX              ",
  "             .....XX            ",
  "             .......XX          ",
  "              ........XX        ",
  "              ..........XX      ",
  "               ...........X     ",
  "               ....XXXXXXXX     ",
  "                ...X            ",
  "                ...X            ",
  "                 ..X            ",
  "                 ..X            ",
  "                  .X            ",
  "                  .X            ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "12,12"
};

static const char *cursor_attach[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "     ...X  ....X  ....X  ...    ",
  "     XXX.X.XXXX.X.XXXX.X.XXX    ",
  "       .....  .....  .....      ",
  "        .X.X   .X.X   .X.X      ",
  "     ...X  ....X  ....X  ...    ",
  "     XXX   XXXX   XXXX   XXX    ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "17,14"
};

static const char *cursor_attack[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "               .                ",
  "               .X               ",
  "            ....X..             ",
  "          ......X....           ",
  "         ...XXX.XXX...          ",
  "        ..XX   .X   X..         ",
  "       ..X     .X     ..        ",
  "      ..X      XX      ..       ",
  "      ..X              ..X      ",
  "     ..X                ..X     ",
  "     ..X                ..X     ",
  "     ..X                ..X     ",
  "   ........X   .    ........X   ",
  "    XXXXXXXX   X     XXXXXXXX   ",
  "     ..X                ..X     ",
  "     ..X                ..X     ",
  "     ..X                ..X     ",
  "      ..                .X      ",
  "      ..X      .       ..X      ",
  "       ..      .X     ..X       ",
  "        ..     .X    ..X        ",
  "         ...   .X   ..X         ",
  "         X......X....X          ",
  "          XX....X..XX           ",
  "            XXX.XXX             ",
  "               .X               ",
  "               XX               ",
  "                                ",
  "                                ",
  "15,15"
};

static const char *cursor_bomb[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                   . .          ",
  "                  . . .         ",
  "                  XXX.          ",
  "               XXXX . .         ",
  "               XX  . .          ",
  "               XX               ",
  "              ....X             ",
  "              ....X             ",
  "            .XXXXXX.XX          ",
  "          ............X         ",
  "         ..............X        ",
  "        ................X       ",
  "       ..................X      ",
  "       ..................X      ",
  "      ....................X     ",
  "      ....................X     ",
  "      ....................X     ",
  "      ....................X     ",
  "      ....................X     ",
  "       ..................X      ",
  "       ..................X      ",
  "        ................X       ",
  "         ..............X        ",
  "          ............X         ",
  "            ........XX          ",
  "            XXXXXXXX            ",
  "                                ",
  "                                ",
  "                                ",
  "16,16"
};

static const char *cursor_bridge[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "           ..                   ",
  "         ....                   ",
  "       .......                  ",
  "     .........                  ",
  "   ............                 ",
  "   ............             ..  ",
  "   X............          .. .  ",
  "   X............        ..XX.   ",
  "    X............     ..XXX.    ",
  "    X............   ..XXXX.     ",
  "     X..............XXXXX.      ",
  "   ..X............XXXXXX.       ",
  "  ....X..........XXXXX..        ",
  "  ....X.........XXXX......      ",
  "  .....X.......XXX.........     ",
  "  .....X......XX............    ",
  "  ......X....XXXXX..........X   ",
  "  ......X...XX..XXXX........X   ",
  " ........X. ..XXX    .......X   ",
  ".........X..XXX      .......X.. ",
  " XX......X XX        ..........X",
  "   XXX...X           .........XX",
  "      XXXX           .........X ",
  "                     ........XX ",
  "                   ..........X  ",
  "                    XX......XX  ",
  "                      XXX...X   ",
  "                         XXXX   ",
  "                                ",
  "                                ",
  "16,16"
};

static const char *cursor_build[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                ......          ",
  "              .....XX..X        ",
  "           .X.....X  XXX        ",
  "          .X......X             ",
  "          X.......X             ",
  "          .......XX             ",
  "         .......X               ",
  "         ......X.X              ",
  "       .......X...X             ",
  "      .......X ....X            ",
  "       ....XX   ....X           ",
  "        ...X     ....X          ",
  "         .X       ....X         ",
  "                   ....X        ",
  "                    ....X       ",
  "                     ...X       ",
  "                      ..X       ",
  "                       X        ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "15,15"
};

static const char *cursor_embark[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "           ...........X         ",
  "           ...........X         ",
  "           ...........X         ",
  "           ...........X         ",
  "        .................X      ",
  "         ...............XX      ",
  "          .............X        ",
  "           ...........X         ",
  "           X.........X          ",
  "        ...XX.......XX...X      ",
  "         ...XX.....XX...X       ",
  "          ...XX...XX...X        ",
  "           ...XX.XX...X         ",
  "            ...XXX...X          ",
  "             ...X...X           ",
  "              .....X            ",
  "               ...X             ",
  "                .X              ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "16,22"
};

static const char *cursor_fix[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "           ...                  ",
  "             ..X                ",
  "              ..X               ",
  "              ..X               ",
  "       .      ..X               ",
  "       .X     ..X               ",
  "       .X    X..X               ",
  "       ..XXXX...X               ",
  "        .........X              ",
  "         .........X             ",
  "              .....X            ",
  "               .....XXXXX       ",
  "                .........X      ",
  "                 .........X     ",
  "                  ...X   ..X    ",
  "                  ..X     .X    ",
  "                  ..X     .X    ",
  "                  ..X           ",
  "                  ..X           ",
  "                   ..XXX        ",
  "                    ...X        ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "16,16"
};

static const char *cursor_guard[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "       .....X      .....        ",
  "       .XXX.........XXX.X       ",
  "       .X..XXXXXXXXX..X.X       ",
  "       .X.............X.X       ",
  "       .X.............X.X       ",
  "       .X.............X.X       ",
  "       .X............X.X        ",
  "        .X...........X.X        ",
  "        .X...........X.X        ",
  "        .XX.........XX.X        ",
  "         .X.........X.X         ",
  "         .XX.......XX.X         ",
  "          .XX.....XX.X          ",
  "           .XX...XX.X           ",
  "            ..XXX..X            ",
  "             .....X             ",
  "               .XX              ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "16,17"
};

static const char *cursor_jam[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "             .....X             ",
  "           ...XXX...X           ",
  "          ..XX   XX..X          ",
  "         ..X       X..X         ",
  "         .X   ...X  X.X         ",
  "        ..X  .X  .X  ..X        ",
  "        .X  .X    .X  .X        ",
  "        .X  .X .X .X  .X        ",
  "        .X  .XX.XX.X  .X        ",
  "        ..X  X...XX  ..X        ",
  "         .X   ...X   .X         ",
  "         ..X .....X ..X         ",
  "          .XX.....XX.X          ",
  "           X.......XX           ",
  "            .......X            ",
  "           .........X           ",
  "           .........X           ",
  "            XXXXXXXXX           ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "15,15"
};

static const char *cursor_lockon[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "             .....X             ",
  "           ...XXX...X           ",
  "          ..XX   XX..X          ",
  "         ..X       X..X         ",
  "         .X   ...X  X.X         ",
  "        ..X  .X  .X  ..X        ",
  "        .X  .X    .X  .X        ",
  "        .X  .X .X .X  .X        ",
  "        .X  .XX.XX.X  .X        ",
  "        ..X  X...XX  ..X        ",
  "         .X   ...X   .X         ",
  "         ..X .....X ..X         ",
  "          .XX.....XX.X          ",
  "           X.......XX           ",
  "            .......X            ",
  "           .........X           ",
  "           .........X           ",
  "            XXXXXXXXX           ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "15,15"
};

static const char *cursor_menu[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "          ..XX                  ",
  "          ....XX                ",
  "           .....XX              ",
  "           .......XX            ",
  "            ........XX          ",
  "            ..........XX        ",
  "             ........XX         ",
  "             .....XX            ",
  "              ...XX             ",
  "              ...XX             ",
  "               ..XX             ",
  "               . X              ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "11,11"
};

static const char *cursor_move[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "           .  .....X .          ",
  "          ..X.XXXXX.X..         ",
  "         ...XXX    XX...        ",
  "        ....X.X    .X....       ",
  "       .....X.X    .X.....      ",
  "        XXXXX.X    .XXXXX       ",
  "        .X....X    ....X.X      ",
  "       .XXXXXXX          .X     ",
  "       .X                .X     ",
  "       .X                .X     ",
  "       .X                .X     ",
  "       .X                .X     ",
  "        .X....     ....X.X      ",
  "        XXXXX.X    .XXXXX       ",
  "       .....X.X    .X.....      ",
  "        ....X.X    .X....X      ",
  "         ...XXX    XX...X       ",
  "          ..X.X    .X..X        ",
  "           .X .....X .X         ",
  "              XXXXX             ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "15,15"
};

static const char *cursor_notpossible[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "        .X            .X        ",
  "       ...X          ...X       ",
  "      .....X        .....X      ",
  "     .......X      .......X     ",
  "      .......X    .......X      ",
  "       .......X  .......X       ",
  "        .......X.......X        ",
  "         .............X         ",
  "          ...........X          ",
  "           .........X           ",
  "            .......X            ",
  "           .........X           ",
  "          ...........X          ",
  "         .............X         ",
  "        .......X.......X        ",
  "       .......X  .......X       ",
  "      .......X    .......X      ",
  "     .......X      .......X     ",
  "      .....X        .....X      ",
  "       ...X          ...X       ",
  "        .X            .X        ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "15,17"
};

static const char *cursor_pickup[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "             .X  .X             ",
  "             .X  .X             ",
  "             .X.X.X             ",
  "              ...X              ",
  "               .X               ",
  "              ...X              ",
  "             .X.X.X             ",
  "             .X  .X             ",
  "             .X.X.X             ",
  "              ...X              ",
  "              X.XX              ",
  "             .X.X.X             ",
  "            ..X.X..X            ",
  "           ...X.X...X           ",
  "          ....X.X....X          ",
  "         .....XXX.....X         ",
  "         ...XXXXXXX...X         ",
  "         .XXX      XX.X         ",
  "         .X          .X         ",
  "         .X          .X         ",
  "         .X          .X         ",
  "         .X          .X         ",
  "         ..X        ..X         ",
  "          ..X      ..X          ",
  "           ..X    ..X           ",
  "            .X    .X            ",
  "                                ",
  "                                ",
  "                                ",
  "15,20"
};

static const char *cursor_seekrepair[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "      ...                       ",
  "        ..X             X       ",
  "         ..X            .X      ",
  "         ..X            ..X     ",
  "  .      ..X        XXXX...X    ",
  "  .X     ..X        ........X   ",
  "  .X    X..X        .........X  ",
  "  ..XXXX...X        ..........  ",
  "   .........X       .........   ",
  "    .........X      ........    ",
  "         .....X         ...     ",
  "          .....XXXXX    ..      ",
  "           .........X   .       ",
  "            .........X          ",
  "             ...X   ..X         ",
  "             ..X     .X         ",
  "             ..X     .X         ",
  "             ..X                ",
  "             ..X                ",
  "              ..XXX             ",
  "               ...X             ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "16,15"
};

static const char *cursor_select[] = {
  /* width height num_colors chars_per_pixel */
  "    32    32        3            1",
  /* colors */
  "X c #000000",
  ". c #ffffff",
  "  c None",
  /* pixels */
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "    .....X            .....X    ",
  "    .XXXXX             XXX.X    ",
  "    .X                    .X    ",
  "    .X                    .X    ",
  "    .X                    .X    ",
  "    XX                    XX    ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "               .X               ",
  "               XX               ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "    .                     .     ",
  "    .X                    .X    ",
  "    .X                    .X    ",
  "    .X                    .X    ",
  "    .....X            .....X    ",
  "    XXXXXX            XXXXXX    ",
  "                                ",
  "                                ",
  "                                ",
  "                                ",
  "15,15"
};

static SDL_Cursor *init_system_cursor(const char *image[])
{
  int i, row, col;
  Uint8 data[4*32];
  Uint8 mask[4*32];
  int hot_x, hot_y;

  i = -1;
  for ( row=0; row<32; ++row ) {
    for ( col=0; col<32; ++col ) {
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
  return SDL_CreateCursor(data, mask, 32, 32, hot_x, hot_y);
}

