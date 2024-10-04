/*  A single-file, libc-only implementation of Tetris for braille displays.  */
/* Meant to be built with Cosmopolitan Libc for a cross-platform fat binary. */

#include <assert.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

enum piece { I, O, L, J, T, S, Z };
enum orientation { DOWN, LEFT, UP, RIGHT };
enum {
  PIECES = Z + 1,
  ROTATIONS = RIGHT + 1,
  SPAWN = DOWN,
  SPAWN_X = 4,
  SPAWN_Y = 2,
  BLOCKS_PER_PIECE = 4,
  WIDTH = 10,
  HEIGHT = 22,
  BRAILLE_CELL_WIDTH = 2,
  BRAILLE_CELL_HEIGHT = 4
};

struct coord { signed char x, y; };

static const struct coord delta[PIECES][ROTATIONS][BLOCKS_PER_PIECE] =
{ [T] = { [SPAWN] = { { -1,  0 }, {  0,  0 }, {  1,  0 }, {  0,  1 } }
        , [LEFT]  = { {  0, -1 }, { -1,  0 }, {  0,  0 }, {  0,  1 } }
        , [UP]    = { { -1,  0 }, {  0,  0 }, {  1,  0 }, {  0, -1 } }
        , [RIGHT] = { {  0, -1 }, {  0,  0 }, {  1,  0 }, {  0,  1 } } }
, [J] = { [SPAWN] = { { -1,  0 }, {  0,  0 }, {  1,  0 }, {  1,  1 } }
        , [LEFT]  = { {  0, -1 }, {  0,  0 }, { -1,  1 }, {  0,  1 } }
        , [UP]    = { { -1, -1 }, { -1,  0 }, {  0,  0 }, {  1,  0 } }
        , [RIGHT] = { {  0, -1 }, {  1, -1 }, {  0,  0 }, {  0,  1 } } }
, [Z] = { [SPAWN] = { { -1,  0 }, {  0,  0 }, {  0,  1 }, {  1,  1 } }
        , [LEFT]  = { {  1, -1 }, {  0,  0 }, {  1,  0 }, {  0,  1 } }
        , [UP]    = { { -1,  0 }, {  0,  0 }, {  0,  1 }, {  1,  1 } }
        , [RIGHT] = { {  1, -1 }, {  0,  0 }, {  1,  0 }, {  0,  1 } } }
, [O] = { [SPAWN] = { { -1,  0 }, {  0,  0 }, { -1,  1 }, {  0,  1 } }
        , [LEFT]  = { { -1,  0 }, {  0,  0 }, { -1,  1 }, {  0,  1 } }
        , [UP]    = { { -1,  0 }, {  0,  0 }, { -1,  1 }, {  0,  1 } }
        , [RIGHT] = { { -1,  0 }, {  0,  0 }, { -1,  1 }, {  0,  1 } } }
, [S] = { [SPAWN] = { {  0,  0 }, {  1,  0 }, { -1,  1 }, {  0,  1 } }
        , [LEFT]  = { {  0, -1 }, {  0,  0 }, {  1,  0 }, {  1,  1 } }
        , [UP]    = { {  0,  0 }, {  1,  0 }, { -1,  1 }, {  0,  1 } }
        , [RIGHT] = { {  0, -1 }, {  0,  0 }, {  1,  0 }, {  1,  1 } } }
, [L] = { [SPAWN] = { { -1,  0 }, {  0,  0 }, {  1,  0 }, { -1,  1 } }
        , [LEFT]  = { { -1, -1 }, {  0, -1 }, {  0,  0 }, {  0,  1 } }
        , [UP]    = { {  1, -1 }, { -1,  0 }, {  0,  0 }, {  1,  0 } }
        , [RIGHT] = { {  0, -1 }, {  0,  0 }, {  0,  1 }, {  1,  1 } } }
, [I] = { [SPAWN] = { { -2,  0 }, { -1,  0 }, {  0,  0 }, {  1,  0 } }
        , [LEFT]  = { {  0, -2 }, {  0, -1 }, {  0,  0 }, {  0,  1 } }
        , [UP]    = { { -2,  0 }, { -1,  0 }, {  0,  0 }, {  1,  0 } }
        , [RIGHT] = { {  0, -2 }, {  0, -1 }, {  0,  0 }, {  0,  1 } } }
};

struct piece_info
{
  enum piece kind:8;
  enum orientation rotation:8;
  signed char x, y;
};

static unsigned short playfield[HEIGHT];

static inline bool test_playfield(struct coord pos)
{ return playfield[pos.y] & (1 << pos.x); }

static inline bool set_playfield(struct coord pos)
{ return playfield[pos.y] |= (1 << pos.x); }

static inline bool clear_playfield(struct coord pos)
{ return playfield[pos.y] &= ~(1 << pos.x); }

static struct piece_info bag[PIECES * 4];
static size_t bag_count = 0;

static struct piece_info random_piece()
{
  if (!bag_count) {
    bag_count = sizeof(bag) / sizeof(*bag);
    for (size_t i = 0; i != bag_count; i++)
      bag[i] = (struct piece_info){ i % PIECES, SPAWN, SPAWN_X, SPAWN_Y };

    for (size_t i = bag_count - 1; i > 0; i--) {
      const size_t j = rand() % (i + 1);
      struct piece_info tmp = bag[i];
      bag[i] = bag[j];
      bag[j] = tmp;
    }
  }

  return bag[--bag_count];
}

static struct piece_info active_piece;

static struct coord active_piece_block(size_t i)
{
  assert(i < BLOCKS_PER_PIECE);
  const struct coord * const offset =
    &delta[active_piece.kind][active_piece.rotation][i];

  return (struct coord){
    .x = active_piece.x - offset->x, .y = active_piece.y - offset->y
  };
}

static void add_active_piece()
{
  for (size_t i = 0; i != BLOCKS_PER_PIECE; i++) {
    struct coord pos = active_piece_block(i);
    assert(!test_playfield(pos));
    set_playfield(pos);
  }
}

static void remove_active_piece()
{
  for (size_t i = 0; i != 4; i++) {
    struct coord pos = active_piece_block(i);
    assert(test_playfield(pos));
    clear_playfield(pos);
  }
}

static bool check_overlap()
{
  for (size_t i = 0; i != BLOCKS_PER_PIECE; i++) {
    const struct coord pos = active_piece_block(i);
    if (pos.x < 0 || pos.x >= WIDTH) return true;
    if (pos.y < 0 || pos.y >= HEIGHT) return true;
    if (test_playfield(pos)) return true;
  }

  return false;
}

static signed char get_active_piece_x_bounds()
{
  signed char result = active_piece.x;
  for (size_t i = 0; i != BLOCKS_PER_PIECE; i++) {
    const signed char x = active_piece_block(i).x;
    if (x < result) result = x;
  }

  return result;
}

volatile bool alarmed;

static void alarm_handler(int sig)
{ if (sig == SIGALRM) alarmed = true; }

static bool is_timer()
{
  if (alarmed) {
    alarmed = false;
    return true;
  }
  return false;
}

static const struct sigaction alarm_action = {.sa_handler = &alarm_handler};

static void initialize_timer()
{
  if (sigaction(SIGALRM, &alarm_action, NULL) == -1) {
    perror("sigaction");
    exit(EXIT_FAILURE);
  }
}

static void set_timer_interval(unsigned int usec)
{
  assert(usec > 0);
  const struct timeval interval = { usec / 1000000, usec % 1000000 };
  const struct itimerval timer = {interval, interval};
  if (setitimer(ITIMER_REAL, &timer, NULL) == -1) {
    perror("setitimer");
    exit(EXIT_FAILURE);
  }  
}

#define CTRL(C) ((C) ^ 0b01000000)

enum {
  ARROW_LEFT = 1000,
  ARROW_RIGHT,
  ARROW_UP,
  ARROW_DOWN,
  INSERT_KEY,
  DELETE_KEY,
  HOME_KEY,
  END_KEY,
  PAGE_UP,
  PAGE_DOWN,
  TICK
};

static int read_event()
{
  static enum { START, ESC_SEQ = '\e', BRACKET_SEQ = '[' } state = START;
  static unsigned short arg = 0;

  while (1) {
    char ch;
    const ssize_t n = read(STDIN_FILENO, &ch, sizeof(ch));

    if (n == -1) {
      if (errno == EINTR) {
        if (is_timer()) return TICK;
        continue;
      }
      perror("read");
      exit(EXIT_FAILURE); // Return -1 on other read errors
    } else if (n == 0) {
      if (state == START) continue;
      state = START;
      return '\e';
    }

    switch (state) {
    case START:
      if (ch == '\e') {
        state = ESC_SEQ;
      } else {
        return ch; // Regular character read
      }
      break;

    case ESC_SEQ:
      if (ch == '[') {
        state = BRACKET_SEQ;
        arg = 0;
      } else {
        state = START;
        return '\e'; // Return ESC if not following with '['
      }
      break;

    case BRACKET_SEQ:
      if (ch >= '0' && ch <= '9') {
        arg = (arg * 10) + (ch - '0');
        break;
      }
      state = START;
      switch (ch) {
      case 'A': return ARROW_UP;
      case 'B': return ARROW_DOWN;
      case 'C': return ARROW_RIGHT;
      case 'D': return ARROW_LEFT;
      case '~':
        switch (arg) {
        case 1: return HOME_KEY;
        case 2: return INSERT_KEY;
        case 3: return DELETE_KEY;
        case 4: return END_KEY;
        case 5: return PAGE_UP;
        case 6: return PAGE_DOWN;
        }
        break;
      }
      break;
    }
  }
}

static struct termios orig_termios;

static void disable_raw_mode()
{ tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios); }

static void atExit(void)
{ disable_raw_mode(); }

void enable_raw_mode()
{
  struct termios raw;

  if (!isatty(STDIN_FILENO)) {
    errno = ENOTTY;
    perror("enable_raw_mode");
    exit(EXIT_FAILURE);
  }
  atexit(atExit);
  if (tcgetattr(STDIN_FILENO, &orig_termios) == -1) {
    perror("tcgetattr");
    exit(EXIT_FAILURE);
  }

  raw = orig_termios;
  /* input modes: no break, no CR to NL, no parity check, no strip char,
   * no start/stop output control. */
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  /* output modes - disable post processing */
  raw.c_oflag &= ~(OPOST);
  /* control modes - set 8 bit chars */
  raw.c_cflag |= (CS8);
  /* local modes - choing off, canonical off, no extended functions,
   * no signal chars (^Z,^C) */
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  /* control chars - set return condition: min number of bytes and timer. */
  raw.c_cc[VMIN] = 0;  /* Return each byte, or zero for timeout. */
  raw.c_cc[VTIME] = 1; /* 100 ms timeout (unit is tens of second). */

  /* put terminal in raw mode after flushing */
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) < 0) {
    perror("tcsetattr");
    exit(EXIT_FAILURE);
  }
}

enum dots
{
  dot1 = 0b00000001, dot2 = 0b00000010, dot3 = 0b00000100, dot4 = 0b00001000,
  dot5 = 0b00010000, dot6 = 0b00100000, dot7 = 0b01000000, dot8 = 0b10000000
};

static const unsigned char brl[BRAILLE_CELL_WIDTH][BRAILLE_CELL_HEIGHT] = {
  { dot4, dot5, dot6, dot8 }, { dot1, dot2, dot3, dot7 }
};

static int score = 0;
static short level = 1;

static inline signed char min(signed char a, signed char b)
{ return a < b ? a : b; }

static void draw_screen()
{
  unsigned char line[(HEIGHT - 2) / 2] = {0};
  const signed char offset = min(get_active_piece_x_bounds(),  6);
  for (signed char y = 2; y != HEIGHT; y++)
    for (signed char x = 0; x != BRAILLE_CELL_HEIGHT; x++)
      if (test_playfield((struct coord){ .x = x + offset, .y = y }))
        line[(HEIGHT - 1 - y) / 2] |= brl[y % BRAILLE_CELL_WIDTH][x];

  unsigned char utf8[sizeof(line) * 3];
  unsigned char *p = &utf8[0];
  for (size_t i = 0; i != sizeof(line); i++) {
    *p++ = 0xE2;
    *p++ = 0xA0 | (line[i] >> 6);
    *p++ = 0x80 | (line[i] & 0x3F);
  }

  write(STDOUT_FILENO, "\e[H", 3);
  write(STDOUT_FILENO, utf8, sizeof(utf8));
  printf("  %d points, level %d\e[1;13H", score, level);
  fflush(stdout);
}

static const char CLEAR_SCREEN[] = "\e[2J";

static void clear_screen()
{
  write(STDOUT_FILENO, CLEAR_SCREEN, sizeof(CLEAR_SCREEN) - 1);
  fflush(stdout);
}

static void new_game()
{
  memset(playfield, 0, sizeof(playfield));
  active_piece = random_piece();
  score = 0;
  level = 1;
  add_active_piece();
}

static inline bool is_complete_line(size_t y)
{
  const unsigned short mask = (1 << WIDTH) - 1;
  return (playfield[y] ^ mask) == 0;
}

static const int points_per_line[] = { 1, 40, 100, 300, 1200 };

static int eliminate_lines()
{
  int lines = 0;

  for (size_t y = 0; y < HEIGHT; y++) {
    if (!is_complete_line(y)) continue;

    for (short h = y; h > 2; h--) playfield[h] = playfield[h - 1];

    lines++;
  }

  return points_per_line[lines];
}

static void handle_piece_bottom()
{
  score += eliminate_lines();

  level = 1 + score / 700;

  active_piece = random_piece();

  if (check_overlap()) {
    new_game();
    return;
  }

  add_active_piece();
}

static bool do_move_down()
{
  bool bottom = false;

  remove_active_piece();
  active_piece.y++;
  if (check_overlap()) {
    bottom = true;
    active_piece.y--;
  }
  add_active_piece();

  /* delay = 800 * pow(0.9, level); */

  return bottom;
}

static void move_down()
{ if (do_move_down()) handle_piece_bottom(); }

static void hard_drop()
{ while (!do_move_down()) continue; handle_piece_bottom(); }

static void move_left()
{
  remove_active_piece();
  active_piece.x--;
  if (check_overlap())
    active_piece.x++;
  add_active_piece();
}

static void move_right()
{
  remove_active_piece();
  active_piece.x++;
  if (check_overlap())
    active_piece.x--;
  add_active_piece();
}

static void rotate()
{
  const enum orientation rotation = active_piece.rotation;
  remove_active_piece();
  active_piece.rotation = (rotation + 1) % ROTATIONS;
  if (check_overlap()) active_piece.rotation = rotation;
  add_active_piece();
}

static void welcome()
{
  clear_screen();
  fputs("\e[2;8H" "Welcome to BETRIS"
        "\e[4;1H" "BETRIS is a clone of a classic puzzle game where geometric shapes, known as"
        "\e[5;1H" "Tetrominos, fall from the right to the left of the screen. Tetrominos are"
        "\e[6;1H" "composed of 4 connected dots. The player's goal is to move and rotate"
        "\e[7;1H" "these Tetrominos to create complete vertical lines, which then disappear,"
        "\e[8;1H" "making space for new Tetrominos. The game ends when there's no more space"
        "\e[9;1H" "for new Tetrominos. The vertical size of the playfield is 10, while the"
        "\e[10;1H" "braille display will only show a 4-dot high section of the playfield."
        "\e[12;1H" "Instructions:"
        "\e[13;1H" "1. Use the following keys to control the pieces:"
        "\e[14;4H"    "- UP ARROW: Move piece up"
        "\e[15;4H"    "- DOWN ARROW: Move piece down"
        "\e[16;4H"    "- RIGHT ARROW or ENTER: Rotate piece"
        "\e[17;4H"    "- LEFT ARROW: Move piece left"
        "\e[18;4H"    "- SPACE: Drop piece to the bottom"
        "\e[19;4H"    "- 'q' or ESC: Quit the game"
        "\e[21;1H" "Press any key to start the game..."
        "\e[21;1H", stdout
  );
  fflush(stdout);
  while (read_event() == TICK) continue;
}

static inline void seed_rng() { srand((unsigned)time(NULL)); }

int main()
{
  seed_rng();
  enable_raw_mode();
  welcome();
  initialize_timer();
  new_game();
  draw_screen();
  set_timer_interval(1000000 * pow(0.9, level));

  for (;;) {
    switch (read_event()) {
    case 'h':
    case ARROW_LEFT:
    case TICK:
      move_down();
      break;
    case 'k':
    case ARROW_UP:
      move_left();
      break;
    case 'j':
    case ARROW_DOWN:
      move_right();
      break;
    case ARROW_RIGHT:
    case CTRL('M'):
      rotate();
      break;
    case ' ':
      hard_drop();
      break;
    case 'q':
    case CTRL('Q'):
    case CTRL('['):
      fputs("\e[2J\e[5;5HThanks for playing betris\r\n\r\n", stdout);
      exit(EXIT_SUCCESS);
    default:
      continue;
    }
    draw_screen();
  }
}
