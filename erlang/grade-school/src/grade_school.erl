%% =============================================================================
%% @author Eric Bailey
%% @copyright 2015 Eric Bailey
%% @version 0.1.0
%% @doc Altering and sorting grade school rosters.
%% @end
%% =============================================================================

-module(grade_school).

-export([new/0, add/3, get/2, sort/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                  TYPES                                   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @type school(). A school is a map (`orddict') from grade to list of students.
-type school() :: orddict:orddict(grade(), [student()]).

%% @type grade(). A grade is an integer.
-type grade() :: integer().

%% @type student(). A student is represented by their first name, as a string.
-type student() :: string().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                   API                                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc The empty {@link school(). school}.
-spec new() -> school().
new() -> orddict:new().

%% @doc Given a {@link grade(). grade}, a {@link student(). student} and a
%% {@link school(). school}, append `[Student]' to the existing list of {@link
%% student(). student}s in `Grade' at `School', or set the list to `[Student]'
%% if none exists.
-spec add(student(), grade(), school()) -> school().
add(Student, Grade, School) ->
    case get(Grade, School) of
        [] ->
            orddict:store(Grade, [Student], School);
        Class ->
            orddict:store(Grade, ordsets:add_element(Student, Class), School)
    end.

%% @doc Given a {@link grade(). grade} and a {@link school(). school}, return
%% the list of {@link student(). student}s in `Grade' at `School'.
-spec get(grade(), school()) -> [student()].
get(Grade, School) ->
    case orddict:find(Grade, School) of
        {ok, Class} -> Class;
        _ -> []
    end.

%% @doc Given a {@link school(). school}, return list of tuples, `[{grade(),
%% [student()]}]', sorted in ascending order by {@link grade(). grade} then by
%% {@link student(). student}. Since {@link school(). school}s are implemented
%% as`orddict's, this is effectively the identity function.
-spec sort(school()) -> school().
sort(School) -> School.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                EMACS CONFIG                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Local Variables:
%% erlang-indent-level: 2
%% End:
