#ifndef COAL_PANIC_H
#define COAL_PANIC_H

/**
 * Terminate the program with an error message.
 *
 * Parameters:
 *   msg - Error message to display
 */
_Noreturn void rt_panic(const char *msg);

/**
 * Mark code path as unreachable.
 * Terminates the program if reached.
 */
_Noreturn void rt_unreachable(void);

#endif
