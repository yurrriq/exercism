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
-export([write/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% buffer record with default current_size of 0,
%% and max_size and queue as undefined
-record(buffer, {current_size = 0         :: buffer_size(),
                 max_size     = undefined :: buffer_size(),
                 queue        = undefined :: queue:queue(term())}).

%% @type buffer_size(). A buffer size is an integer.
-type buffer_size() :: integer().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                              SYNCHRONOUS API                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Given a buffer size, create a new circular buffer of that size and
%% return its pid.
%%
%% @see size/1
-spec create(buffer_size()) -> pid().
create(Size) ->
  {ok, Pid} = gen_server:start_link(?MODULE, [Size], []),
  Pid.

%% @doc Given a circular buffer pid, synchronously get and return its size.
-spec size(pid()) -> {ok, buffer_size()}.
size(Pid) -> gen_server:call(Pid, size).

%% @doc Given a circular buffer pid, synchronously pop the first item off its
%% queue. If the queue is empty, return `{error, empty}'.
-spec read(pid()) -> {error, empty} | {ok, term()}.
read(Pid) -> gen_server:call(Pid, read).

%% @doc Given a circular buffer pid and an item, synchronously attempt to write
%% the item to the buffer's queue. If the queue is full, return `{error, full}',
%% otherwise return `ok'.
%%
%% @see write/2
-spec write_attempt(pid(), term()) -> {error, full} | 'ok'.
write_attempt(Pid, Item) -> gen_server:call(Pid, {write_attempt, Item}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                              ASYNCHRONOUS API                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Given a circular buffer pid and an item, asynchronously insert the item
%% at the rear of the buffer's queue, first removing the item at the front of
%% the queue if it's full.
%%
%% @see write_attempt/2
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
handle_call(read, _From, #buffer{current_size = N, queue = Q1} = Buffer) ->
  {{value, Item}, Q2} = queue:out(Q1),
  {reply, {ok, Item}, Buffer#buffer{current_size = N - 1, queue = Q2}};
handle_call(size, _From, #buffer{max_size = Size} = Buffer) ->
  {reply, {ok, Size}, Buffer};
handle_call({write_attempt, _Item}, _From, #buffer{max_size     = N,
                                                   current_size = N} = Buffer) ->
  {reply, {error, full}, Buffer};
handle_call({write_attempt, Item}, _From, Buffer) ->
  {reply, ok, enqueue(Buffer, Item)}.

%% @private
handle_cast({write, Item}, Buffer)  -> {noreply, enqueue(Buffer, Item)}.

%% @private
handle_info(timeout, State)         -> {noreply, State}.

%% @private
terminate(_Reason, _State)          -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                PRIVATE API                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @see write/2
-spec enqueue(#buffer{}, term()) -> #buffer{}.
enqueue(#buffer{current_size = N, max_size = N, queue = Q1} = Buffer, Item) ->
  {_Removed, Q2} = queue:out(Q1),
  Buffer#buffer{queue = queue:in(Item, Q2)};
enqueue(#buffer{current_size = N, queue = Q1} = Buffer, Item) ->
  Buffer#buffer{current_size = N + 1, queue = queue:in(Item, Q1)}.
