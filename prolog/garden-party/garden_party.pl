/*
\PL*/
:- use_module(library(clpfd)).
/*PL

\Predicate garden_party/3(Pairs, Dishes, Beverages).

Four chefs, Aisha, Emma, Mei, and Winona, are attending a garden party. They
each prepare a different dish and bring a different beverage.

\PL*/
garden_party(Pairs, Dishes, Beverages) :-
    Table = [Chefs, Dishes, Beverages],
    ChefNames = [aisha, emma, mei, winona],
    Chefs = [Aisha, Emma, Mei, Winona],
    Dishes = [PadThai, Frybread, Tagine, Biryani],
    Beverages = [Tonic, Lassi, _Kombucha, Amasi],
    pairs_keys_values(Pairs, Chefs, ChefNames),
    maplist(all_distinct, Table),
    append(Table, Vs),
    Vs ins 1..4,
/*PL

Aisha prepares Tagine.

\PL*/
    Aisha #= Tagine,
/*PL

Emma brings Amasi.

\PL*/
    Emma #= Amasi,
/*PL

The chef who prepares Frybread brings Tonic.

\PL*/
    Frybread #= Tonic,
/*PL

Mei brings Lassi.

\PL*/
    Mei #= Lassi,
/*PL

Winona does not prepare Pad Thai.

\PL*/
    Winona #\= PadThai,
/*PL

The chef who brings the Lassi did not cook the Biryani

\PL*/
    Lassi #\= Biryani.
/*PL

\Predicate dish/2(Chef, Dish).

\PL*/
dish(Chef, Dish) :-
    DishNames = [pad_thai, frybread, tagine, biryani],
    garden_party(ChefPairs, Dishes, _),
    label(Dishes),
    pairs_keys_values(DishPairs, Dishes, DishNames),
    member((N-Chef), ChefPairs),
    member((N-Dish), DishPairs),
    !.
/*PL

\Predicate beverage/2(Chef, Beverage).

\PL*/

beverage(Chef, Beverage) :-
    BeverageNames = [tonic, lassi, kombucha, amasi],
    garden_party(ChefPairs, _, Beverages),
    label(Beverages),
    pairs_keys_values(BeveragePairs, Beverages, BeverageNames),
    member((N-Chef), ChefPairs),
    member((N-Beverage), BeveragePairs),
    !.
/*PL
\EndProlog*/
