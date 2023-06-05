#include "resistor_color.h"

static resistor_band_t all_colors[10] = {COLORS};

resistor_band_t *colors()
{
    return all_colors;
}

int color_code(resistor_band_t color)
{
    return color;
}
