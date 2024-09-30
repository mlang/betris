#include <assert.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <termios.h>
#include <unistd.h>

enum piece { I, O, L, J, T, S, Z };
enum { PIECES = Z + 1, ROTATIONS = 4, WIDTH = 10, HEIGHT = 22 };

static const unsigned short tetrimino[PIECES][ROTATIONS] =
{ [I] = { 0x8888, 0xF000, 0x8888, 0xF000 }
, [O] = { 0xCC00, 0xCC00, 0xCC00, 0xCC00 }
, [L] = { 0xC880, 0xE200, 0x44C0, 0x8E00 }
, [J] = { 0xC440, 0x2E00, 0x88C0, 0xE800 }
, [T] = { 0x4E00, 0x8C80, 0xE400, 0x4C40 }
, [S] = { 0x4C80, 0xC600, 0x4C80, 0xC600 }
, [Z] = { 0x8C40, 0x6C00, 0x8C40, 0x6C00 }
};

static inline bool test_piece(enum piece kind, unsigned char rotation, short y, short x)
{ return tetrimino[kind][rotation] & (0x8000 >> (y*4 + x)); }

struct piece_info
{
  enum piece kind:8;
  unsigned char rotation;
  signed char x, y;
};

static struct piece_info random_piece()
{
  struct piece_info info = { rand() % PIECES, rand() % ROTATIONS, 4, 3 };
  return info;
}

static unsigned short board[HEIGHT];

static inline bool test_board(short y, short x)
{ return board[y] & (1 << x); }

static inline bool set_board(short y, short x)
{ return board[y] |= (1 << x); }

static inline bool clear_board(short y, short x)
{ return board[y] &= ~(1 << x); }

static struct piece_info current_piece, next_piece;

static void add_current_piece()
{
  for (short y = 0; y != 4; y++)
    for (short x = 0; x != 4; x++)
      if (test_piece(current_piece.kind, current_piece.rotation, y, x))
        set_board(current_piece.y - y, current_piece.x + x);
}

static void remove_current_piece()
{
  for (short y = 0; y != 4; y++)
    for (short x = 0; x != 4; x++)
      if (test_piece(current_piece.kind, current_piece.rotation, y, x)) {
        assert(test_board(current_piece.y - y, current_piece.x + x));
        clear_board(current_piece.y - y, current_piece.x + x);
      }
}

static bool check_overlap()
{
  for (short y = 0; y != 4; y++)
    for (short x = 0; x != 4; x++)
      if (test_piece(current_piece.kind, current_piece.rotation, y, x)) {
        const short gy = current_piece.y - y, gx = current_piece.x + x;

        if (gy >= HEIGHT || gy < 0 || gx >= WIDTH || gx < 0) return true;
        if (test_board(gy, gx)) return true;
      }

  return false;
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

static void set_timer_interval(int usec)
{
  struct timeval interval = { 0, usec };
  while (interval.tv_usec >= 1000000) {
    interval.tv_sec += 1;
    interval.tv_usec -= 1000000;
  }
  struct itimerval timer = {interval, interval};
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
  DEL_KEY,
  HOME_KEY,
  END_KEY,
  PAGE_UP,
  PAGE_DOWN,
  TICK
};

enum step_name { S_start, S_has_char };

struct state_info
{
  enum step_name step;
  char c;
} state = { S_start };

static int read_k()
{
  int nread;
  switch (state.step) {
  case S_start:
    do {
      nread = read(STDIN_FILENO, &state.c, 1);
      if (nread == -1) {
	if (errno == EINTR && is_timer()) return TICK;
	perror("read");
	exit(EXIT_FAILURE);
      }
    } while (nread == 0);
    state.step = S_has_char;
  case S_has_char:
    switch (state.c) {
    case CTRL('J'):
      state.step = S_start;
      return CTRL('M');
    case CTRL('V'):
      state.step = S_start;
      return PAGE_DOWN;
    default:
      state.step = S_start;
      return state.c;
    }
  }
}

    
static int read_key()
{
  int nread;
  char c, seq[3];
  do {
    nread = read(STDIN_FILENO, &c, 1);
    if (nread == -1) {
      if (errno == EINTR && is_timer()) return TICK;
      perror("read");
      exit(EXIT_FAILURE);
    }
  } while (!nread);

  while (1) {
    switch (c) {
      case CTRL('J'): /* newline */
        return CTRL('M');
      case CTRL('V'):
        return PAGE_DOWN;
      case '\e': /* escape sequence */
        nread = read(STDIN_FILENO, seq, 1);
        if (nread == -1) {
          if (errno == EINTR && is_timer()) return TICK;
          perror("read");
          exit(EXIT_FAILURE);
        }
        /* If this is just an ESC, we'll timeout here. */
        if (nread == 0) return CTRL('[');

        if (seq[0] == '[') {
          if (read(STDIN_FILENO, seq + 1, 1) == 0)
            return CTRL('[');
          if (seq[1] >= '0' && seq[1] <= '9') {
            /* Extended escape, read additional byte. */
            if (read(STDIN_FILENO, seq + 2, 1) == 0)
              return CTRL('[');
            if (seq[2] == '~') {
              switch (seq[1]) {
                case '1':
                  return HOME_KEY;
                case '3':
                  return DEL_KEY;
                case '4':
                  return END_KEY;
                case '5':
                  return PAGE_UP;
                case '6':
                  return PAGE_DOWN;
              }
            }
          } else {
            /* Arrow Keys
             *
             *   KEY   CODE     FN  SHIFT OPTION
             * ───── ────── ────── ────── ──────
             *    UP    ←[A   ←[5~    ←[A    ←[A
             *  DOWN    ←[B   ←[6~    ←[B    ←[B
             * RIGHT    ←[C   ←[4~ ←[1;2C    ←[f
             *  LEFT    ←[D   ←[1~ ←[1;2C    ←[b
             */
            switch (seq[1]) {
              case 'A':
                return ARROW_UP;
              case 'B':
                return ARROW_DOWN;
              case 'C':
                return ARROW_RIGHT;
              case 'D':
                return ARROW_LEFT;
              case 'H':
                return HOME_KEY;
              case 'F':
                return END_KEY;
            }
          }
        } else if (seq[0] == 'v') {
          return PAGE_UP;
        } else if (seq[0] == 'O') {
          if (read(STDIN_FILENO, seq + 1, 1) == 0)
            return CTRL('[');
          /* ESC O sequences. */
          switch (seq[1]) {
            case 'H':
              return HOME_KEY;
            case 'F':
              return END_KEY;
          }
        }
        break;
      default:
        return c;
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

static const unsigned char brl[2][4] = {
  { dot4, dot5, dot6, dot8 }, { dot1, dot2, dot3, dot7 }
};

static int score = 0;
static short level = 1;

static void draw_screen()
{
  unsigned char line[11] = {0};
  short offset = current_piece.x > 6? 6: current_piece.x;
  for (short y = 0; y != HEIGHT; y++)
    for (short x = 0; x != 4; x++)
      if (test_board(y, x + offset))
        line[(HEIGHT - 1 - y) / 2] |= brl[y % 2][x];

  unsigned char utf8[sizeof(line) * 3];
  for (short i = 0; i != sizeof(line); i++) {
    unsigned char *p = &utf8[i * (sizeof(utf8) / sizeof(line))];
    *p++ = 0xE2;
    *p++ = 0xA0 | (line[i] >> 6);
    *p++ = 0x80 | (line[i] & 0x3F);
  }

  write(STDOUT_FILENO, "\e[2J\e[H", 7);
  write(STDOUT_FILENO, utf8, sizeof(utf8));
  printf("  %d points, level %d\e[1;14H", score, level);
  fflush(stdout);
}

static void new_game()
{
  memset(board, 0, sizeof(board));
  current_piece = random_piece();
  next_piece = random_piece();
  score = 0;
  level = 1;
  add_current_piece();
}

static inline bool is_complete_line(short y)
{
  const unsigned short mask = (1 << WIDTH) - 1;
  return (board[y] ^ mask) == 0;
}

static const int points_per_line[] = { 1, 40, 100, 300, 1200 };

static int eliminate_lines()
{
  int lines = 0;

  for (short y = 0; y < HEIGHT; y++) {
    if (!is_complete_line(y)) continue;

    for (short h = y; h > 2; h--) board[h] = board[h - 1];

    lines++;
  }

  return points_per_line[lines];
}

static void handle_piece_bottom()
{
  score += eliminate_lines();

  level = 1 + score / 700;

  current_piece = next_piece;
  next_piece = random_piece();

  if (check_overlap()) {
    new_game();
    return;
  }

  add_current_piece();
}

static bool do_move_down()
{
  bool bottom = false;

  remove_current_piece();
  current_piece.y++;
  if (check_overlap()) {
    bottom = true;
    current_piece.y--;
  }
  add_current_piece();

  /* delay = 800 * pow(0.9, level); */

  return bottom;
}

static void move_down()
{ if (do_move_down()) handle_piece_bottom(); }

static void move_bottom()
{ while (!do_move_down()); handle_piece_bottom(); }

static void move_left()
{
  remove_current_piece();
  current_piece.x--;
  if (check_overlap())
    current_piece.x++;
  add_current_piece();
}

static void move_right()
{
  remove_current_piece();
  current_piece.x++;
  if (check_overlap())
    current_piece.x--;
  add_current_piece();
}

static void rotate()
{
  const int rotation = current_piece.rotation;
  remove_current_piece();
  current_piece.rotation = (rotation + 1) % 4;
  if (check_overlap()) current_piece.rotation = rotation;
  add_current_piece();
}

static void welcome()
{
  printf("\e[2J"
         "\e[2;8H" "Welcome to Braille Tetris (BETRIS)"
         "\e[4;1H" "Instructions:"
         "\e[5;1H" "1. The pieces \"fall\" from right to left."
         "\e[6;1H" "2. The display shows a 4-dot high section of the board."
         "\e[7;1H" "3. Use the following keys to control the pieces:"
         "\e[8;4H"    "- 'h' or LEFT ARROW: Move piece down"
         "\e[9;4H"    "- 'k' or UP ARROW: Move piece left"
         "\e[10;4H"   "- 'j' or DOWN ARROW: Move piece right"
         "\e[11;4H"   "- RIGHT ARROW or ENTER: Rotate piece"
         "\e[12;4H"   "- SPACE: Drop piece to the bottom"
         "\e[13;4H"   "- 'q' or ESC: Quit the game"
         "\e[15;1H" "Press any key to start the game..."
         "\e[15;1H"
  );
  fflush(stdout);
  read_key();
}

int main()
{
  enable_raw_mode();
  welcome();
  initialize_timer();
  new_game();
  draw_screen();
  set_timer_interval(800000 * pow(0.9, level));

  for (;;) {
    switch (read_key()) {
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
      move_bottom();
      break;
    case 'q':
    case CTRL('Q'):
    case CTRL('['):
      write(STDOUT_FILENO, "\e[2J\e[5;5HThanks for playing betris\r\n\r\n", 39);
      exit(EXIT_SUCCESS);
    }
    draw_screen();
  }
}
