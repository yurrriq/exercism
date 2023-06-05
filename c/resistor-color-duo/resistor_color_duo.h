#ifndef RESISTOR_COLOR_DUO_H
#define RESISTOR_COLOR_DUO_H
#include <stdint.h>

#define COLORS BLACK, BROWN, RED, ORANGE, YELLOW, GREEN, BLUE, VIOLET, GREY, WHITE

typedef enum RESISTOR_BANDS {
    COLORS
} resistor_band_t;

uint16_t color_code(resistor_band_t colors[2]);
#endif
