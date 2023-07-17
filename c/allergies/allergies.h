/**
 * @file allergies.h
 * @brief Determine and compute lists of allergies.
 * @author Eric Bailey
 * @date 2023-07-16
 */

#ifndef ALLERGIES_H
#define ALLERGIES_H

#include <stdbool.h>
#include <stdint.h>

//! Possible allergens.
typedef enum {
    /// Eggs
    ALLERGEN_EGGS = 0,
    /// Peanuts
    ALLERGEN_PEANUTS,
    /// Shellfish
    ALLERGEN_SHELLFISH,
    /// Strawberries
    ALLERGEN_STRAWBERRIES,
    /// Tomatoes
    ALLERGEN_TOMATOES,
    /// Chocolate
    ALLERGEN_CHOCOLATE,
    /// Pollen
    ALLERGEN_POLLEN,
    /// Cats
    ALLERGEN_CATS,
    /// The number of possible allergens.
    ALLERGEN_COUNT,
} allergen_t;

/**
 * @struct allergen_list_t
 * A list of allergies.
 *
 * @var allergen_list_t::count
 *   The number of allergies, i.e. true values in @allergens
 * @var allergen_list_t::allergens
 *   Whether or not each allergy is present.
 */
typedef struct {
    int count;
    bool allergens[ALLERGEN_COUNT];
} allergen_list_t;

bool is_allergic_to(const allergen_t item, const uint16_t score);

allergen_list_t get_allergens(const uint16_t score);

#endif
