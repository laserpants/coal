#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>
#include <sys/select.h>
#include <time.h>

// Event loop runtime
//
// Event encoding (int32):
//   0         -- tick (timer fired)
//   1 .. 255  -- key press (ASCII code)
//   Arrow keys encoded as: A=Up(65), B=Down(66), C=Right(67), D=Left(68)
//
// Interface:
//   event_loop_init     : int32 -> unit   (tick interval in ms)
//   event_loop_has_next : unit  -> bool
//   event_loop_next     : unit  -> int32

#define EVENT_TICK 0
#define MAX_EVENTS 256

static int32_t g_queue[MAX_EVENTS];
static int g_head = 0;
static int g_tail = 0;
static int32_t g_tick_ms = 1000;
static struct termios g_orig_termios;
static struct timespec g_last_tick = {0, 0};

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

static void enqueue(int32_t v) { g_queue[g_tail++ % MAX_EVENTS] = v; }
static int32_t dequeue(void) { return g_queue[g_head++ % MAX_EVENTS]; }

static void restore_terminal(void)
{
    tcsetattr(STDIN_FILENO, TCSANOW, &g_orig_termios);
}

// Collect any events that are ready right now without blocking.
static void collect_events(void)
{
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);

    // Initialise last_tick on first call.
    if (g_last_tick.tv_sec == 0 && g_last_tick.tv_nsec == 0)
        g_last_tick = now;

    // Tick event if enough time has elapsed.
    long elapsed_ms = (now.tv_sec - g_last_tick.tv_sec) * 1000L + (now.tv_nsec - g_last_tick.tv_nsec) / 1000000L;
    if (elapsed_ms >= g_tick_ms)
    {
        enqueue(EVENT_TICK);
        g_last_tick = now;
    }

    // Key-press event via non-blocking stdin read.
    fd_set fds;
    FD_ZERO(&fds);
    FD_SET(STDIN_FILENO, &fds);
    struct timeval zero = {0, 0};

    if (select(STDIN_FILENO + 1, &fds, NULL, NULL, &zero) > 0)
    {
        unsigned char c;
        if (read(STDIN_FILENO, &c, 1) == 1)
        {
            // Handle arrow key escape sequences: ESC [ A/B/C/D
            if (c == 27) // ESC
            {
                unsigned char seq[2];
                // Try to read next two bytes non-blocking
                if (read(STDIN_FILENO, &seq[0], 1) == 1 && seq[0] == '[' &&
                    read(STDIN_FILENO, &seq[1], 1) == 1)
                {
                    // Arrow key: emit the letter code (A/B/C/D)
                    if (seq[1] >= 'A' && seq[1] <= 'D')
                    {
                        enqueue((int32_t)seq[1]);
                    }
                    else
                    {
                        // Unknown escape sequence, emit ESC
                        enqueue(27);
                    }
                }
                else
                {
                    // Just ESC key by itself
                    enqueue(27);
                }
            }
            else
            {
                enqueue((int32_t)c);
            }
        }
    }
}

// How many milliseconds until the next scheduled tick?
static long ms_until_tick(void)
{
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    long elapsed_ms = (now.tv_sec - g_last_tick.tv_sec) * 1000L + (now.tv_nsec - g_last_tick.tv_nsec) / 1000000L;
    long remaining = g_tick_ms - elapsed_ms;
    return remaining > 0 ? remaining : 0;
}

// ---------------------------------------------------------------------------
// Coal externals
// ---------------------------------------------------------------------------

// event_loop_init(tick_ms) -- tick interval in milliseconds
void *event_loop_init(int32_t tick_ms)
{
    g_tick_ms = tick_ms;
    srand((unsigned)time(NULL));

    // Switch stdin to raw mode: no line buffering, no echo.
    tcgetattr(STDIN_FILENO, &g_orig_termios);
    atexit(restore_terminal);

    struct termios raw = g_orig_termios;
    raw.c_lflag &= ~(ICANON | ECHO);
    raw.c_cc[VMIN] = 0; // non-blocking reads
    raw.c_cc[VTIME] = 0;
    tcsetattr(STDIN_FILENO, TCSANOW, &raw);

    return NULL; // unit
}

// event_loop_has_next() -- blocks until a key press or tick is available,
// then returns true. Returns false only on stdin error/close.
_Bool event_loop_has_next(void *_unit)
{
    while (g_head >= g_tail)
    {
        long wait_ms = ms_until_tick();

        fd_set fds;
        FD_ZERO(&fds);
        FD_SET(STDIN_FILENO, &fds);
        struct timeval tv = {wait_ms / 1000, (wait_ms % 1000) * 1000L};

        if (select(STDIN_FILENO + 1, &fds, NULL, NULL, &tv) < 0)
        {
            return false; // stdin closed or error
        }

        collect_events();
    }
    return true;
}

// event_loop_next() -- dequeue and return the next event
int32_t event_loop_next(void *_unit)
{
    return dequeue();
}

// ---------------------------------------------------------------------------
// Board rendering
// ---------------------------------------------------------------------------

// Minimal struct overlays matching the LLVM IR layouts for Main.Cons / Nil /
// Pair.  int32 fields are boxed as inline pointer casts (no heap allocation),
// so extracting them is just (int32_t)(uintptr_t)ptr.
//
//   %Main.Cons  = { i32 tag=0, ptr head, ptr tail }
//   %Main.Nil   = { i32 tag=1 }
//   %Main.Pair  = { i32 tag=0, ptr x,    ptr y    }

typedef struct
{
    int32_t tag;
    void *head;
    void *tail;
} coal_list_t;
typedef struct
{
    int32_t tag;
    void *x;
    void *y;
} coal_pair_t;

static void mark_cells(void *list, char grid[20][10], char mark)
{
    coal_list_t *node = (coal_list_t *)list;
    while (node && node->tag == 0)
    { /* 0 = Cons, 1 = Nil */
        coal_pair_t *pair = (coal_pair_t *)node->head;
        int32_t x = (int32_t)(uintptr_t)pair->x;
        int32_t y = (int32_t)(uintptr_t)pair->y;
        if (x >= 0 && x < 10 && y >= 0 && y < 20)
            grid[y][x] = mark;
        node = (coal_list_t *)node->tail;
    }
}

// coal_render_board(board, block_cells)
// Locked board cells shown as '#', falling block as '@', empty as '.'.
void *coal_render_board(void *board_list, void *block_cells_list)
{
    char grid[20][10] = {0};
    mark_cells(board_list, grid, 1);       /* locked cells  */
    mark_cells(block_cells_list, grid, 2); /* falling piece */

    fputs("\033[2J\033[H", stdout); /* clear screen, cursor home */
    fputs("+--------------------+\n", stdout);
    for (int y = 0; y < 20; y++)
    {
        putchar('|');
        for (int x = 0; x < 10; x++)
        {
            char c = grid[y][x] == 1 ? '#' : grid[y][x] == 2 ? '@'
                                                             : '.';
            putchar(c);
            putchar(c);
        }
        puts("|");
    }
    fputs("+--------------------+\n", stdout);
    fflush(stdout);
    return NULL; /* unit */
}

// ---------------------------------------------------------------------------
// Random block selection
// ---------------------------------------------------------------------------

// random_shape() -- returns a random int in 0..6 (one index per Shape)
int32_t random_shape(void *_unit)
{
    return rand() % 7;
}

// random_rotation() -- returns a random int in 0..3 (one index per Rotation)
int32_t random_rotation(void *_unit)
{
    return rand() % 4;
}
