#include "reverse_string.h"
#include <stdlib.h>
#include <string.h>

char *reverse(const char *str)
{
    if (str == NULL && *str == '\0')
        return "";

    size_t len = strlen(str);
    char *rstr = calloc(len + 1, sizeof(str[0]));

    for (size_t i = 0; i < len; i++)
        rstr[i] = str[len - 1 - i];

    return rstr;
}
