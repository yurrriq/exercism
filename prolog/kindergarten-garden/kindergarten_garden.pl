/*

\PrologDialect{swiprolog}

\Predicate plant/2(?Plant, ?Encoding).
\label{pred:plant/2}

Four different types of seeds are planted:

\begin{table}[h!]
  \begin{tabular}{ l l l }
    \hline
    Plant & Prolog encoding & Diagram encoding \\
    \hline
    Grass & \verb|grass| & G \\
    Clover & \verb|clover| & C \\
    Radish & \verb|radishes| & R \\
    Violets & \verb|violet| & V
  \end{tabular}
\end{table}

\PL*/
plant(grass, 'G').
plant(clover, 'C').
plant(radishes, 'R').
plant(violets, 'V').
/*PL

\Predicate find_child_plants/3(?Child, ?FirstRow, ?SecondRow, ?Children, ?Plants).
\label{pred:find_child_plants/3}

Find \verb|Child|'s \verb|Plants|, given the two rows of plants and list of
\verb|Children|. \\

If \verb|Child| is the first of \verb|Children|, their \verb|Plants| are the
first two in each row.

\PL*/
find_child_plants(Child, [P1, P2  |_], [P3, P4  |_],
                  [Child  |_], [P1, P2, P3, P4]) :-
    !.
/*PL

Otherwise, recursively check the next set of plants.

\PL*/
find_child_plants(Child, [_, _  |FirstRow], [_, _  |SecondRow],
                  [_  |Children], Plants) :-
    find_child_plants(Child, FirstRow, SecondRow, Children, Plants).
/*PL

\Predicate garden/3(?Garden, ?Child, ?Plants).
\label{pred:garden/3}

Determine which \verb|Plants| in the \verb|Garden| belong to the given
\verb|Child|.

\PL*/
garden(Garden, Child, Plants) :-
/*PL

List all the \verb|Children|.

\PL*/
    Children = [alice, bob, charlie, david, eve, fred,
                ginny, harriet, ileana, joseph, kincaid, larry],
/*PL

Split the \verb|Garden| into its two lines.
\PL*/
    split_string(Garden, "\n", "", [FirstLine, SecondLine]),
/*PL

Convert both lines into rows of encoded plants.
\PL*/
    string_chars(FirstLine, FirstRow),
    string_chars(SecondLine, SecondRow),
/*PL

Determine which \verb|EncodedPlants| belong to the given \verb|Child|.
\PL*/
    find_child_plants(Child, FirstRow, SecondRow, Children, EncodedPlants),
/*PL

Convert the list of \verb|EncodedPlants| into a list \verb|Plants| of plant
names.

\PL*/
    maplist(plant, Plants, EncodedPlants).
/*PL

\EndProlog*/
