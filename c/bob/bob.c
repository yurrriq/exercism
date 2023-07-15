#include "bob.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>

extern char *strdup(const char *);
extern size_t strlen(const char *);

bool is_empty(const char *str);
bool is_question(const char *str);
bool is_yelled(const char *str);
char *trim(const char *str);
char *rtrim(const char *str);
char *ltrim(const char *str);

const char *hey_bob(const char *greeting)
{
    const char *trimmed = trim(greeting);

    if (is_empty(trimmed))
        return "Fine. Be that way!";

    bool question = is_question(trimmed);
    bool yelled = is_yelled(trimmed);

    if (question && yelled) {
        return "Calm down, I know what I'm doing!";
    }
    else if (question) {
        return "Sure.";
    }
    else if (yelled) {
        return "Whoa, chill out!";
    }
    else {
        return "Whatever.";
    }

    return "";
}

bool is_empty(const char *str)
{
    return !str || *str == '\0';
}

bool is_question(const char *str)
{
    return str[strlen(str) - 1] == '?';
}

bool is_yelled(const char *str)
{
    if (str == NULL)
        return true;

    bool has_letter = false;
    for (size_t i = 0; i < strlen(str); i++) {
        char c = str[i];

        if (isalpha(c))
            has_letter = true;

        if (c != toupper(c))
            return false;
    }

    return has_letter;
}

char *trim(const char *str)
{
    char *tmp = rtrim(str);
    char *trimmed = ltrim(tmp);
    free(tmp);

    return trimmed;
}

char *rtrim(const char *str)
{
    while (isspace(*str))
        str++;

    return strdup(str);
}

char *ltrim(const char *str)
{
    char *rstr = strdup(str);
    if (rstr == NULL)
        return rstr;

    char *end = rstr + strlen(str) - 1;
    while (end > rstr && isspace(*end))
        end--;

    *++end = 0;

    return rstr;
}
