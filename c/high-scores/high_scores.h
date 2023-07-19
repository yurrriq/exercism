#ifndef HIGH_SCORES_H
/**
 * @file high_scores.h
 * @brief Manage a game player's high score list.
 * @author Eric Bailey
 * @date 2023-07-18
 */

#define HIGH_SCORES_H

#include <stddef.h>
#include <stdint.h>

/**
 * Return the latest score.
 *
 * @param scores The high score list.
 * @param scores_len The number of scores.
 *
 * @return The latest score.
 */
int32_t latest(const int32_t *scores, size_t scores_len);

/**
 * Return the highest score.
 *
 * @param scores The high score list.
 * @param scores_len The number of scores.
 *
 * @return The highest score.
 */
int32_t personal_best(const int32_t *scores, size_t scores_len);

/**
 * Write the highest scores to @output in descending order.
 *
 * @param scores The high score list.
 * @param scores_len The number of scores.
 * @param output A pointer to write the highest scores list to.
 *
 * @return The number of scores written.
 */
size_t personal_top_three(const int32_t *scores, size_t scores_len, int32_t *output);

#endif
