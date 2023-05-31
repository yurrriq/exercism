-module(allergies).

-export([allergies/1, is_allergic_to/2]).

-define(ALLERGIES, [
    eggs,
    peanuts,
    shellfish,
    strawberries,
    tomatoes,
    chocolate,
    pollen,
    cats
]).

allergies(Score) ->
    lists:filter(fun(A) -> is_allergic_to(A, Score) end, ?ALLERGIES).

is_allergic_to(Allergen, Score) ->
    case index_of(Allergen, ?ALLERGIES) of
        not_found -> false;
        Index -> (Score band trunc(math:pow(2, Index))) > 0.0
    end.

index_of(E, L) -> index_of(E, L, 0).

index_of(_, [], _) -> not_found;
index_of(E, [E | _], I) -> I;
index_of(E, [_ | T], I) -> index_of(E, T, I + 1).
