#include "bob.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>

extern char *strdup(const char *);
extern size_t strlen(const char *);

bool is_question(char *str);
bool is_yelled(char *str);
char *trim(char *str);
char *rtrim(char *str);
char *ltrim(char *str);

char *hey_bob(char *greeting)
{
    char *trimmed = trim(greeting);

    if (trimmed[0] == '\0')
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

bool is_question(char *str)
{
    return str[strlen(str) - 1] == '?';
}

bool is_yelled(char *str)
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

char *trim(char *str)
{
    char *tmp = rtrim(str);
    char *trimmed = ltrim(tmp);
    free(tmp);

    return trimmed;
}

char *rtrim(char *str)
{
    while (isspace(*str))
        str++;

    return strdup(str);
}

char *ltrim(char *str)
{
    char *rstr = strdup(str);
    if (rstr == NULL)
        return str;

    char *end = rstr + strlen(str) - 1;
    while (end > rstr && isspace(*end))
        end--;

    *++end = 0;

    return rstr;
}
