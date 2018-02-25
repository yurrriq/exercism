#include "acronym.h"
#include <ctype.h>
#include <stdlib.h>


static int is_word_start(char current, char previous)
{
    return isalpha(current) && (!isalpha(previous));
}


static int word_count(const char phrase[])
{
    if (phrase == NULL)
        return 0;

    int count = 1;

    for (size_t i = 1; phrase[i] != '\0'; ++i) {
        if (is_word_start(phrase[i], phrase[i - 1]))
            count++;
    }

    return count;
}


char *abbreviate(const char *phrase)
{
    if (phrase == NULL || phrase[0] == '\0')
        return NULL;

    char *acronym = NULL;
    if (!(acronym = calloc(word_count(phrase) + 1, sizeof phrase[0])))
        return NULL;

    acronym[0] = toupper(phrase[0]);

    for (size_t i = 1, j = 1; phrase[i] != '\0'; ++i) {
        if (is_word_start(phrase[i], phrase[i - 1]))
            acronym[j++] = toupper(phrase[i]);
    }

    return acronym;
}
