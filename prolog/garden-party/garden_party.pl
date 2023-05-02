solve(Solution) :-
    Solution = [
        [aisha, Bervage1, Dish1],
        [emma, Beverage2, Dish2],
        [mei, Beverage3, Dish3],
        [winona, Beverage4, Dish4]
    ],
    member([aisha, _, tagine], Solution),
    member([emma, amasi, DishAmasi], Solution),
    member([_, tonic, frybread], Solution),
    member([mei, lassi, _], Solution),
    member([winona, _, DishWinona], Solution),
    permutation([Beverage1, Beverage2, Beverage3, Beverage4],
                [tonic, lassi, kombucha, amasi]),
    permutation([Dish1, Dish2, Dish3, Dish4],
                [pad_thai, frybread, tagine, biryani]),
    DishWinona \= pad_thai,
    DishAmasi \= biryani,
    !.

beverage(Chef, Beverage) :-
    solve(Solution),
    member([Chef, Beverage, _], Solution),
    !.

%% beverage(emma, amasi).
%% beverage(mei, lassi).
%% beverage(Chef, amasi) :-
%%     \+ dish(Chef, biryani).

dish(Chef, Dish) :-
    solve(Solution),
    member([Chef, _, Dish], Solution),
    !.

%% dish(aisha, tagine).
%% dish(Chef, frybread) :-
%%     beverage(Chef, tonic).
%% dish(Chef, pad_thai) :-
%%     \+ (Chef = winona).
