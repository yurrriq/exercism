/**
 * @file allergies.c
 * @brief Determine and compute lists of allergies.
 * @author Eric Bailey
 * @date 2023-07-16
 */

#include "allergies.h"

/**
 * Given an item and an allergy score, determine whether the person is allergic.
 *
 * @param item A potential allergen.
 * @param score The results of an allergy test.
 *
 * @return Whether the person is allergic to the @item.
 */
bool is_allergic_to(const allergen_t item, const uint16_t score)
{
    return score & (1 << item);
}

/**
 * Compute a person's full list of allergies.
 *
 * @param score The results of an allergy test.
 *
 * @return The full list of allergies.
 */
allergen_list_t get_allergens(const uint16_t score)
{
    allergen_list_t results;
    results.count = 0;

    for (allergen_t item = 0; item < ALLERGEN_COUNT; item++) {
        if ((results.allergens[item] = is_allergic_to(item, score)))
            results.count++;
    }

    return results;
}
