%% =============================================================================
%% @author Eric Bailey
%% @doc Creating and manipulating circular buffers.
%% See the <a href="overview-summary.html">overview</a> for more information.
%% @end
%% =============================================================================

-module(circular_buffer).

-behaviour(gen_server).

%% Synchronous API
-export([create/1, size/1, read/1, write_attempt/2]).

%% Asynchronous API
-export ([write/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(buffer, {current_size = 0         :: integer(),
                 max_size     = undefined :: integer(),
                 queue        = undefined :: queue:queue(term())}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                              SYNCHRONOUS API                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec create (integer()) -> pid().
create(Size) ->
  {ok, Pid} = gen_server:start_link(?MODULE, [Size], []),
  Pid.

size(Pid) -> gen_server:call(Pid, size).

read(Pid) -> gen_server:call(Pid, read).

write_attempt(Pid, Item) -> gen_server:call(Pid, {write_attempt, Item}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                              ASYNCHRONOUS API                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec write(pid(), term()) -> 'ok'.
write(Pid, Item) -> gen_server:cast(Pid, {write, Item}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                            GEN_SERVER CALLBACKS                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
init([MaxSize]) ->
  {ok, #buffer{max_size = MaxSize,
               queue    = queue:new()}}.

%% @private
handle_call(read, _From, #buffer{current_size = 0} = Buffer) ->
  {reply, {error, empty}, Buffer};
handle_call(read, _From, #buffer{current_size = CurrentSize,
                                 queue        = Queue1} = Buffer) ->
  {{value, Item}, Queue2} = queue:out(Queue1),
  {reply, {ok, Item}, Buffer#buffer{current_size = CurrentSize - 1,
                                    queue        = Queue2}};
handle_call(size, _From, #buffer{max_size = Size} = Buffer) ->
  {reply, {ok, Size}, Buffer};
handle_call({write_attempt, Item}, _From, #buffer{max_size     = MaxSize,
                                                  current_size = CurrentSize,
                                                  queue        = Queue1} = Buffer) ->
  case CurrentSize =:= MaxSize of
    true ->
      {reply, {error, full}, Buffer};
    false ->
      {reply, ok, Buffer#buffer{current_size = CurrentSize + 1,
                                queue        = queue:in(Item, Queue1)}}
  end.

%% @private
handle_cast({write, Item}, #buffer{current_size = CurrentSize,
                                   max_size     = MaxSize,
                                   queue        = Queue1} = Buffer) ->
  case CurrentSize < MaxSize of
    true ->
      {noreply, Buffer#buffer{current_size = CurrentSize + 1,
                              queue        = queue:in(Item, Queue1)}};
    false ->
      {_Removed, Queue2} = queue:out(Queue1),
      {noreply, Buffer#buffer{current_size = MaxSize,
                              queue        = queue:in(Item, Queue2)}}
  end.

%% @private
handle_info(timeout, State)         -> {noreply, State}.

%% @private
terminate(_Reason, _State)          -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.
