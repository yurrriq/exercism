#include "reverse_string.h"
#include <string>

using namespace std;

namespace reverse_string {

string reverse_string(string str)
{
    if (str.empty()) {
        return str;
    }

    for (int i = 0, j = str.size() - 1; i != j; ++i, --j) {
        swap(str[i], str[j]);
    }

    return str;
}

} // namespace reverse_string
