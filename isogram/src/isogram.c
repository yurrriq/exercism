#include "isogram.h"
#include <stddef.h>
#include <stdint.h>
#include <ctype.h>


bool is_isogram(const char phrase[])
{
    if (phrase == NULL)
        return false;

    uint32_t seen = 0;

    for (size_t i = 0; phrase[i] != '\0'; ++i) {
        if (!isalpha(phrase[i]))
            continue;

        uint32_t bit_mask = 1 << (toupper(phrase[i]) - 'A');
        if (seen & bit_mask)
            return false;

        seen |= bit_mask;
    }

    return true;
}
