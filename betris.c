/*  A single-file, libc-only implementation of Tetris for braille displays.  */
/* ------------------------------------------------------------------------- */
/* Meant to be built with Cosmopolitan Libc for a cross-platform fat binary. */

#include <assert.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

#define ARRAY_SIZE(a) (sizeof(a) / sizeof(*a))
#define BITS(n) ((1U << n) - 1)
#define CSI "\e["
#define ERASE_DISPLAY CSI "2J"
#define ERASE_END_OF_LINE CSI "K"
#define GOTO(y, x) CSI #y ";" #x "H"
#define GOTO_TOP_LEFT CSI "H"
#define HIDE_CURSOR CSI "?25l"
#define SHOW_CURSOR CSI "?25h"

enum piece { I, O, L, J, T, S, Z };
enum orientation { DOWN, LEFT, UP, RIGHT };
enum {
  PIECES = Z + 1,
  MAX_ROTATIONS = RIGHT + 1,
  SPAWN = DOWN,
  SPAWN_X = 4,
  SPAWN_Y = 2,
  BLOCKS_PER_PIECE = 4,
  WIDTH = 10,
  HEIGHT = 22,
  BRAILLE_CELL_WIDTH = 2,
  BRAILLE_CELL_HEIGHT = 4,
  UNICODE_BRAILLE = 0x2800,
  UPPER_HALF_BLOCK = 0x2580,
  LOWER_HALF_BLOCK = 0x2584,
  FULL_BLOCK = 0x2588
};

struct coord { signed char x, y; };

static const struct coord delta[PIECES][MAX_ROTATIONS][BLOCKS_PER_PIECE - 1] =
{ [T] = { [SPAWN] = { { -1,  0 }, {  1,  0 }, {  0,  1 } }
        , [LEFT]  = { {  0, -1 }, { -1,  0 }, {  0,  1 } }
        , [UP]    = { { -1,  0 }, {  1,  0 }, {  0, -1 } }
        , [RIGHT] = { {  0, -1 }, {  1,  0 }, {  0,  1 } } }
, [J] = { [SPAWN] = { { -1,  0 }, {  1,  0 }, {  1,  1 } }
        , [LEFT]  = { {  0, -1 }, { -1,  1 }, {  0,  1 } }
        , [UP]    = { { -1, -1 }, { -1,  0 }, {  1,  0 } }
        , [RIGHT] = { {  0, -1 }, {  1, -1 }, {  0,  1 } } }
, [Z] = { [SPAWN] = { { -1,  0 }, {  0,  1 }, {  1,  1 } }
        , [LEFT]  = { {  1, -1 }, {  1,  0 }, {  0,  1 } }
        , [UP]    = { { -1,  0 }, {  0,  1 }, {  1,  1 } }
        , [RIGHT] = { {  1, -1 }, {  1,  0 }, {  0,  1 } } }
, [O] = { [SPAWN] = { { -1,  0 }, { -1,  1 }, {  0,  1 } }
        , [LEFT]  = { { -1,  0 }, { -1,  1 }, {  0,  1 } }
        , [UP]    = { { -1,  0 }, { -1,  1 }, {  0,  1 } }
        , [RIGHT] = { { -1,  0 }, { -1,  1 }, {  0,  1 } } }
, [S] = { [SPAWN] = { {  1,  0 }, { -1,  1 }, {  0,  1 } }
        , [LEFT]  = { {  0, -1 }, {  1,  0 }, {  1,  1 } }
        , [UP]    = { {  1,  0 }, { -1,  1 }, {  0,  1 } }
        , [RIGHT] = { {  0, -1 }, {  1,  0 }, {  1,  1 } } }
, [L] = { [SPAWN] = { { -1,  0 }, {  1,  0 }, { -1,  1 } }
        , [LEFT]  = { { -1, -1 }, {  0, -1 }, {  0,  1 } }
        , [UP]    = { {  1, -1 }, { -1,  0 }, {  1,  0 } }
        , [RIGHT] = { {  0, -1 }, {  0,  1 }, {  1,  1 } } }
, [I] = { [SPAWN] = { { -2,  0 }, { -1,  0 }, {  1,  0 } }
        , [LEFT]  = { {  0, -2 }, {  0, -1 }, {  0,  1 } }
        , [UP]    = { { -2,  0 }, { -1,  0 }, {  1,  0 } }
        , [RIGHT] = { {  0, -2 }, {  0, -1 }, {  0,  1 } } }
};

struct piece_info
{
  enum piece kind:8;
  enum orientation rotation:8;
  struct coord pos;
};

/* --- Bag of pieces --- */

static struct piece_info bag[PIECES];
static size_t bag_count = 0;

static inline void fill_bag()
{
  while (bag_count != ARRAY_SIZE(bag)) {
    bag[bag_count] = (struct piece_info) {
      bag_count % PIECES, SPAWN, { SPAWN_X, SPAWN_Y }
    };
    bag_count += 1;
  }
}

static inline void shuffle_bag()
{
  for (size_t i = bag_count - 1; i > 0; i--) {
    const size_t j = rand() % (i + 1);

    if (i != j) {
      const struct piece_info tmp = bag[i];
      bag[i] = bag[j];
      bag[j] = tmp;
    }      
  }
}

static struct piece_info random_piece()
{
  if (!bag_count) {
    fill_bag();
    shuffle_bag();
  }

  return bag[--bag_count];
}

static struct piece_info active_piece;

static struct coord active_piece_block(size_t block)
{
  assert(block < BLOCKS_PER_PIECE);

  if (block == 0) return active_piece.pos;

  block -= 1;

  const struct coord offset =
    delta[active_piece.kind][active_piece.rotation][block];

  return (struct coord){
    .x = active_piece.pos.x - offset.x,
    .y = active_piece.pos.y - offset.y
  };
}

static unsigned short playfield[HEIGHT];

static inline bool test_playfield(struct coord pos)
{ return playfield[pos.y] & (1 << pos.x); }

static inline bool set_playfield(struct coord pos)
{ return playfield[pos.y] |= (1 << pos.x); }

static inline bool clear_playfield(struct coord pos)
{ return playfield[pos.y] &= ~(1 << pos.x); }

static void add_active_piece()
{
  for (size_t i = 0; i != BLOCKS_PER_PIECE; i++)
    set_playfield(active_piece_block(i));
}

static void remove_active_piece()
{
  for (size_t i = 0; i != BLOCKS_PER_PIECE; i++)
    clear_playfield(active_piece_block(i));
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
  signed char result = active_piece.pos.x;
  for (size_t i = 0; i != BLOCKS_PER_PIECE; i++) {
    const signed char x = active_piece_block(i).x;
    if (x < result) result = x;
  }

  return result;
}

/* --- I/O --- */

volatile bool alarmed;

static void alarm_handler(int sig)
{ if (sig == SIGALRM) alarmed = true; }

static bool timer_occured()
{
  if (alarmed) {
    alarmed = false;
    return true;
  }
  return false;
}

static void die(const char *msg)
{
  perror(msg);
  exit(EXIT_FAILURE);
}

static void initialize_timer()
{
  const struct sigaction alarm_action = {.sa_handler = &alarm_handler};

  if (sigaction(SIGALRM, &alarm_action, NULL) == -1) die(__FUNCTION__);
}

static void set_timer_interval(unsigned int usec)
{
  assert(usec > 0);
  const struct timeval interval = { usec / 1000000, usec % 1000000 };
  const struct itimerval timer = {interval, interval};
  if (setitimer(ITIMER_REAL, &timer, NULL) == -1) die(__FUNCTION__);
}

enum event {
  RETURN = '\r',
  ESCAPE = '\e',
  CTRL_Q = '',
  SPACE = ' ',
  SMALL_LETTER_H = 'h',
  SMALL_LETTER_J = 'j',
  SMALL_LETTER_K = 'k',
  SMALL_LETTER_L = 'l',
  SMALL_LETTER_P = 'p',
  SMALL_LETTER_Q = 'q',
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

static enum event read_event()
{
  static enum { START, ESC_SEQ = '\e', BRACKET_SEQ = '[' } state = START;
  static unsigned short arg = 0;

  while (1) {
    char ch;
    const ssize_t n = read(STDIN_FILENO, &ch, sizeof(ch));

    if (n == -1) {
      if (errno == EINTR) {
        if (timer_occured()) return TICK;
        continue;
      }
      die("read");
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
    die(__FUNCTION__);
  }
  atexit(atExit);
  if (tcgetattr(STDIN_FILENO, &orig_termios) == -1) die("tcgetattr");

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
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) < 0) die("tcsetattr");
}

enum dots
{
  dot1 = 0b00000001, dot2 = 0b00000010, dot3 = 0b00000100, dot4 = 0b00001000,
  dot5 = 0b00010000, dot6 = 0b00100000, dot7 = 0b01000000, dot8 = 0b10000000
};

static const unsigned char brl[BRAILLE_CELL_WIDTH][BRAILLE_CELL_HEIGHT] = {
  { dot4, dot5, dot6, dot8 }, { dot1, dot2, dot3, dot7 }
};

static size_t score = 0;
static size_t lines_cleared = 0;
static size_t level = 1;

static inline signed char min(signed char a, signed char b)
{ return a < b ? a : b; }

static char *encode_utf8(char *utf8, uint32_t codepoint)
{
  if (codepoint <= BITS(7)) {
    *utf8++ = codepoint;
  } else if (codepoint <= BITS(11)) {
    *utf8++ = 0xC0 | ((codepoint >> 6) & BITS(5));
    *utf8++ = 0x80 | (codepoint & BITS(6));
  } else if (codepoint <= BITS(16)) {
    *utf8++ = 0xE0 | ((codepoint >> 12) & BITS(4));
    *utf8++ = 0x80 | ((codepoint >> 6) & BITS(6));
    *utf8++ = 0x80 | (codepoint & BITS(6));
  } else {
    assert(codepoint <= BITS(21));
    *utf8++ = 0xF0 | ((codepoint >> 18) & BITS(3));
    *utf8++ = 0x80 | ((codepoint >> 12) & BITS(6));
    *utf8++ = 0x80 | ((codepoint >> 6) & BITS(6));
    *utf8++ = 0x80 | (codepoint & BITS(6));
  }

  return utf8;
}

static void draw_screen()
{
  unsigned char line[(HEIGHT - 2) / 2] = {0};
  const signed char offset = min(get_active_piece_x_bounds(),  6);
  for (signed char y = 2; y != HEIGHT; y++)
    for (signed char x = 0; x != BRAILLE_CELL_HEIGHT; x++)
      if (test_playfield((struct coord){ .x = x + offset, .y = y }))
        line[(HEIGHT - 1 - y) / 2] |= brl[y % BRAILLE_CELL_WIDTH][x];

  char utf8[3 + sizeof(line) * 3 + 6 + 100 * 3 + 42];
  char *p = &utf8[0];
  p = stpcpy(p, HIDE_CURSOR GOTO_TOP_LEFT);
  for (size_t i = 0; i != sizeof(line); i++)
    p = encode_utf8(p, UNICODE_BRAILLE | line[i]);

  p += sprintf(p, "  %lu %lu %lu", score, lines_cleared, level);
  p = stpcpy(p, ERASE_END_OF_LINE);

  p = stpcpy(p, "\r\n\r\n\r\n");

  for (size_t y = 2; y != HEIGHT; y += 2) {
    for (size_t x = 0; x != WIDTH; x++) {
      const struct coord upper = { x, y }, lower = { x, y + 1 };
      uint32_t glyph = SPACE;

      if (test_playfield(upper) && test_playfield(lower)) {
        glyph = FULL_BLOCK;
      } else if (test_playfield(upper)) {
        glyph = UPPER_HALF_BLOCK;
      } else if (test_playfield(lower)) {
        glyph = LOWER_HALF_BLOCK;
      }
      p = encode_utf8(p, glyph);
    }
    p = stpcpy(p, "\r\n");
  }

  p = stpcpy(p, GOTO(1, 13) SHOW_CURSOR);

  if (write(STDOUT_FILENO, utf8, p - utf8) != p - utf8) die(__FUNCTION__);
  if (fflush(stdout) == EOF) die(__FUNCTION__);
}

static void new_game()
{
  memset(playfield, 0, sizeof(playfield));
  active_piece = random_piece();
  score = 0;
  level = 1;
  lines_cleared = 0;
  add_active_piece();
}

static bool is_complete_line(size_t y)
{ return (playfield[y] ^ BITS(WIDTH)) == 0; }

static const int points_per_line[] = { 40, 100, 300, 1200 };

static int eliminate_lines()
{
  size_t lines = 0;

  for (size_t y = 0; y < HEIGHT; y++) {
    if (!is_complete_line(y)) continue;

    for (short h = y; h > 2; h--) playfield[h] = playfield[h - 1];

    lines++;
  }

  lines_cleared += lines;

  return lines? points_per_line[lines - 1] * level: 0;
}

static void adjust_speed()
{ set_timer_interval(1000000 * pow(0.95, level)); }

static void handle_piece_bottom()
{
  score += eliminate_lines();
  level = 1 + lines_cleared / 10;
  adjust_speed();

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
  active_piece.pos.y++;
  if (check_overlap()) {
    bottom = true;
    active_piece.pos.y--;
  }
  add_active_piece();

  return bottom;
}

static void move_down()
{ if (do_move_down()) handle_piece_bottom(); }

static void hard_drop()
{ while (!do_move_down()) continue; handle_piece_bottom(); }

static void move_left()
{
  remove_active_piece();
  active_piece.pos.x--;
  if (check_overlap())
    active_piece.pos.x++;
  add_active_piece();
}

static void move_right()
{
  remove_active_piece();
  active_piece.pos.x++;
  if (check_overlap())
    active_piece.pos.x--;
  add_active_piece();
}

static void rotate()
{
  const enum orientation rotation = active_piece.rotation;
  remove_active_piece();
  active_piece.rotation = (rotation + 1) % MAX_ROTATIONS;
  if (check_overlap()) active_piece.rotation = rotation;
  add_active_piece();
}

static void welcome()
{
  if (fputs(ERASE_DISPLAY
            GOTO(2, 8) "Welcome to BETRIS"
            GOTO(4, 1) "BETRIS is a clone of a classic puzzle game where geometric shapes, known as"
            GOTO(5, 1) "Tetrominos, fall from the right to the left of the screen. Tetrominos are"
            GOTO(6, 1) "composed of 4 connected dots. The player's goal is to move and rotate"
            GOTO(7, 1) "these Tetrominos to create complete vertical lines, which then disappear,"
            GOTO(8, 1) "making space for new Tetrominos. The game ends when there's no more space"
            GOTO(9, 1) "for new Tetrominos. The vertical size of the playfield is 10, while the"
            GOTO(10, 1) "braille display will only show a 4-dot high section of the playfield."
            GOTO(12, 1) "Instructions:"
            GOTO(13, 1) "1. Use the following keys to control the pieces:"
            GOTO(14, 4)    "- UP ARROW: Move piece up"
            GOTO(15, 4)    "- DOWN ARROW: Move piece down"
            GOTO(16, 4)    "- RIGHT ARROW or ENTER: Rotate piece"
            GOTO(17, 4)    "- LEFT ARROW: Move piece left"
            GOTO(18, 4)    "- SPACE: Drop piece to the bottom"
            GOTO(19, 4)    "- 'p': Pause the game"
            GOTO(20, 4)    "- 'q' or ESC: Quit the game"
            GOTO(22, 1) "Press any key to start the game..."
            GOTO(22, 1), stdout
      ) == EOF) die(__FUNCTION__);
  if (fflush(stdout) == EOF) die(__FUNCTION__);
  while (read_event() == TICK) continue;
  if (fputs(ERASE_DISPLAY, stdout) == EOF) die(__FUNCTION__);
  if (fflush(stdout) == EOF) die(__FUNCTION__);
}

static inline void seed_rng() { srand((unsigned)time(NULL)); }

int main()
{
  seed_rng();
  enable_raw_mode();
  welcome();
  initialize_timer();
  new_game();
  adjust_speed();

  for (;;) {
    draw_screen();

    switch (read_event()) {
    case SMALL_LETTER_H:
    case ARROW_LEFT:
    case TICK:
      move_down();
      break;
    case SMALL_LETTER_K:
    case ARROW_UP:
      move_left();
      break;
    case SMALL_LETTER_J:
    case ARROW_DOWN:
      move_right();
      break;
    case SMALL_LETTER_L:
    case ARROW_RIGHT:
    case RETURN:
      rotate();
      break;
    case SPACE:
      hard_drop();
      break;
    case SMALL_LETTER_P:
      while (read_event() != SMALL_LETTER_P) continue;
      break;
    case SMALL_LETTER_Q:
    case CTRL_Q:
    case ESCAPE:
      if (fputs("\e[2J\e[5;5HThanks for playing betris\r\n\r\n", stdout) == EOF)
        die("fputs");
      exit(EXIT_SUCCESS);
    default:
      continue;
    }
    draw_screen();
  }
}
