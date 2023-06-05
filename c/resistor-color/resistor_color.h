#ifndef RESISTOR_COLOR_H
#define RESISTOR_COLOR_H

#define COLORS BLACK, BROWN, RED, ORANGE, YELLOW, GREEN, BLUE, VIOLET, GREY, WHITE

typedef enum {
    COLORS
} resistor_band_t;

int color_code(resistor_band_t);

resistor_band_t *colors();
#endif
