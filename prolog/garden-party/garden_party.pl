/*

\Predicate solve/1(Solution).

\PL*/
solve(Solution) :-
/*PL

Four chefs, Aisha, Emma, Mei, and Winona, are attending a garden party. They
each prepare a different dish and bring a different beverage.
\PL*/
    Solution = [
        [aisha, Dish1, Beverage1],
        [emma, Dish2, Beverage2],
        [mei, Dish3, Beverage3],
        [winona, Dish4, Beverage4]
    ],
/*PL

The dishes are Pad Thai, Frybread, Tagine, and Biryani.

\PL*/
    permutation([Dish1, Dish2, Dish3, Dish4],
                [pad_thai, frybread, tagine, biryani]),
/*PL

The beverages are Tonic, Lassi, Kombucha, and Amasi.
\PL*/
    permutation([Beverage1, Beverage2, Beverage3, Beverage4],
                [tonic, lassi, kombucha, amasi]),
/*PL

Aisha prepares tagine.

\PL*/
    member([aisha, tagine, _], Solution),
/*PL

Emma brings amasi.

\PL*/
    member([emma, _, amasi], Solution),
/*PL

The chef who prepares frybread brings tonic.

\PL*/
    member([_, frybread, tonic], Solution),
/*PL

Mei brings lassi.

\PL*/
    member([mei, DishLassi, lassi], Solution),
/*PL

The chef who brings the lassi did not cook the biryani.

\PL*/
    dif(DishLassi, biryani),
/*PL

Winona does not prepare pad thai.

\PL*/
    member([winona, DishWinona, _], Solution),
    dif(DishWinona, pad_thai),
    !.
/*PL

\Predicate beverage/2(Chef, Beverage).

\PL*/
beverage(Chef, Beverage) :-
    solve(Solution),
    member([Chef, _, Beverage], Solution),
    !.
/*PL

\Predicate dish/2(Chef, Dish).

\PL*/
dish(Chef, Dish) :-
    solve(Solution),
    member([Chef, Dish, _], Solution),
    !.
/*PL
\EndProlog*/
