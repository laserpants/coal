#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>
#include <sys/select.h>
#include <time.h>
#include <errno.h>

static struct termios g_orig_termios;

static void restore_terminal(void)
{
    tcsetattr(STDIN_FILENO, TCSANOW, &g_orig_termios);
}

// Collect any events that are ready right now without blocking.
static int32_t poll_kbd()
{
    fd_set fds;
    FD_ZERO(&fds);
    FD_SET(STDIN_FILENO, &fds);
    struct timeval zero = {0, 0};

    int sel_ret;
    do {
        sel_ret = select(STDIN_FILENO + 1, &fds, NULL, NULL, &zero);
    } while (sel_ret < 0 && errno == EINTR);

    if (sel_ret > 0) {
        unsigned char c;
        int n = read(STDIN_FILENO, &c, 1);
        if (n == 1) {
            // Handle arrow key escape sequences: ESC [ A/B/C/D
            if (c == 27) { // ESC
                unsigned char seq[2];
                if (read(STDIN_FILENO, &seq[0], 1) == 1 && seq[0] == '[' &&
                    read(STDIN_FILENO, &seq[1], 1) == 1) {
                    if (seq[1] >= 'A' && seq[1] <= 'D')
                        return (int32_t)seq[1];
                    return 27; // unknown escape, return ESC
                }
                return 27; // just ESC
            }
            return (int32_t)c;
        }
        // n == 0 means EOF (stdin pipe closed)
        // n < 0 means read error
        // Return -1 to signal EOF/error to the Coal layer
        return -1;
    }
    return 0; // no event
}

void *event_loop_init(void *p)
{
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

// event_loop_poll_kbd() -- non-blocking, returns key code or 0 or -1 for EOF
int32_t event_loop_poll_kbd(void *_unit)
{
    return poll_kbd();
}
