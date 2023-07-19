/**
 * @file high_scores.c
 * @brief Manage a game player's high score list.
 * @author Eric Bailey
 * @date 2023-07-18
 */

#include "high_scores.h"

#include <stdlib.h>
#include <string.h>

int ge(const void *x, const void *y);

int32_t latest(const int32_t *scores, size_t scores_len)
{
    return scores[scores_len - 1];
}

int32_t personal_best(const int32_t *scores, size_t scores_len)
{
    int32_t best = scores[0];

    for (size_t i = 0; i < scores_len; i++) {
        int32_t score = scores[i];
        if (score > best)
            best = score;
    }

    return best;
}

size_t personal_top_three(const int32_t *scores, size_t scores_len, int32_t *output)
{
    int32_t sorted[scores_len];
    memcpy(sorted, scores, scores_len * sizeof(int32_t));
    qsort((void *)sorted, scores_len, sizeof(int32_t), ge);

    size_t output_len = scores_len < 3 ? scores_len : 3;
    memcpy(output, sorted, output_len * sizeof(int32_t));

    return output_len;
}

int ge(const void *x, const void *y)
{
    return (*(int32_t *)x < *(int32_t *)y) - (*(int32_t *)x > *(int32_t *)y);
}
