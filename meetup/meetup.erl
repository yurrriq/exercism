-module(meetup).

-export([schedule/4]).

-define(WEEKDAY_NUM, #{ monday    => 1
                      , tuesday   => 2
                      , wednesday => 3
                      , thursday  => 4
                      , friday    => 5
                      , saturday  => 6
                      , sunday    => 7
                      }).

-define(SCHEDULE_NUM, #{ first  => 0
                       , second => 1
                       , third  => 2
                       , fourth => 3
                       , teenth => 4
                       , last   => 5
                       }).

-type year()        :: non_neg_integer().
-type month()       :: 1..12.
-type day()         :: 1..31.
-type daynum()      :: 1..7.
-type date()        :: {year(),month(),day()}.
-type schedulenum() :: 0..5.

-type weekday()     :: monday
                     | tuesday
                     | wednesday
                     | thursday
                     | friday
                     | saturday
                     | sunday.

-type schedule()    :: first | second | third | fourth | teenth | last.

-spec weekday_num(weekday()) -> daynum().
weekday_num(Weekday) -> maps:get(Weekday, ?WEEKDAY_NUM).

-spec schedule_num(schedule()) -> schedulenum().
schedule_num(Schedule) -> maps:get(Schedule, ?SCHEDULE_NUM).

-spec schedule(year(), month(), weekday(), schedule()) -> date().
schedule(Year, Month, Weekday, last) ->
  LastDay = calendar:last_day_of_the_month(Year, Month),
  date_in_week(Weekday, {Year,Month,LastDay-6});
schedule(Year, Month, Weekday, teenth) ->
  date_in_week(Weekday, {Year,Month,13});
schedule(Year, Month, Weekday, Nth) ->
  StartDay = 1 + schedule_num(Nth)*7,
  date_in_week(Weekday, {Year,Month,StartDay}).

-spec date_in_week(Weekday, StartDate) -> date() when
    Weekday    :: weekday(),
    StartDate  :: date().
date_in_week(Weekday, {_,_,Day} = StartDate) ->
  date_in_range(weekday_num(Weekday), StartDate, Day + 6).

-spec date_in_range(DayNum, StartDate, EndDay) -> date() when
    DayNum      :: daynum(),
    StartDate   :: date(),
    EndDay      :: day().
date_in_range(DayNum, {Year,Month,Day} = Date, EndDay) when Day =< EndDay ->
  case calendar:day_of_the_week(Date) of
    DayNum -> Date;
    _      -> date_in_range(DayNum, {Year,Month,Day+1}, EndDay)
  end.
