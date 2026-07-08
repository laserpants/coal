#include "coal/char.h"
#include <wctype.h>

int
rt_char_cmp(uint32_t a, uint32_t b)
{
    if (a < b) {
        return -1;
    }
    if (a > b) {
        return 1;
    }
    return 0;
}

bool
rt_char_is_digit(uint32_t cp)
{
    return (bool) iswdigit((wint_t) cp);
}

bool
rt_char_is_alpha(uint32_t cp)
{
    return (bool) iswalpha((wint_t) cp);
}

bool
rt_char_is_whitespace(uint32_t cp)
{
    return (bool) iswspace((wint_t) cp);
}

bool
rt_char_is_upper(uint32_t cp)
{
    return (bool) iswupper((wint_t) cp);
}

bool
rt_char_is_lower(uint32_t cp)
{
    return (bool) iswlower((wint_t) cp);
}

uint32_t
rt_char_to_upper(uint32_t cp)
{
    return (uint32_t) towupper((wint_t) cp);
}

uint32_t
rt_char_to_lower(uint32_t cp)
{
    return (uint32_t) towlower((wint_t) cp);
}
