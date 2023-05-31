%% =============================================================================
%% @author Eric Bailey
%% @doc Creating, managing and closing bank accounts.
%% @end
%% =============================================================================

-module(bank_account).

-behaviour(gen_server).

%% Sychronous API
-export([create/0, balance/1, charge/2, close/1, withdraw/2]).

%% Asychronous API
-export([deposit/2]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% account record with default balance of 0
-record(account, {balance = 0 :: amount()}).

%% @type amount(). An amount is an `integer()'.
-type amount() :: integer().

%% @doc Create a new bank account and return its pid.
-spec create() -> pid().
create() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Pid.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                              SYNCHRONOUS API                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Given a bank account pid, return the current balance.
-spec balance(pid()) -> amount() | {error, atom()}.
balance(Pid) -> gen_server:call(Pid, balance).

%% @doc Given a bank account pid and an {@link amount(). amount},
%% if the amount is not positive, leave the account unchanged and return 0;
%% otherwise if the current balance is greater than the given amount,
%% subtract the amount from the balance, and return the amount,
%% otherwise leave the account unchanged and return 0.
-spec charge(pid(), amount()) -> amount() | {error, atom()}.
charge(Pid, Amount) -> gen_server:call(Pid, {charge, Amount}).

%% @doc Given a bank account pid, close the account and return the balance.
%% Any future transaction will result in an error.
-spec close(pid()) -> amount() | {error, atom()}.
close(Pid) -> gen_server:call(Pid, close).

%% @doc Given a bank account pid and an {@link amount(). amount},
%% if the amount is not positive, leave the account unchanged and return 0;
%% otherwise if the current balance is greater than the given amount,
%% subtract the amount from the balance, and return the amount,
%% otherwise set the balance to 0 and return the current balance.
-spec withdraw(pid(), amount()) -> amount() | {error, atom()}.
withdraw(Pid, Amount) -> gen_server:call(Pid, {withdraw, Amount}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                              ASYNCHRONOUS API                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Given a bank account pid and an {@link amount(). amount},
%%
-spec deposit(pid(), amount()) -> ok.
deposit(Pid, Amount) -> gen_server:cast(Pid, {deposit, Amount}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                            GEN_SERVER CALLBACKS                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
init([]) -> {ok, #account{}}.

%% @private
handle_call(balance, _From, Account = #account{balance = Balance}) ->
    {reply, Balance, Account};
handle_call({charge, Amount}, _From, Account) when Amount =< 0 ->
    {reply, 0, Account};
handle_call({charge, Amount}, _From, Account = #account{balance = Balance}) ->
    case Balance > Amount of
        true -> {reply, Amount, Account#account{balance = Balance - Amount}};
        false -> {reply, 0, Account}
    end;
handle_call(close, _From, #account{balance = Balance}) ->
    {reply, Balance, account_closed};
handle_call({withdraw, Amount}, _From, Account) when Amount =< 0 ->
    {reply, 0, Account};
handle_call({withdraw, Amount}, _From, Account = #account{balance = Balance}) ->
    case Balance > Amount of
        true -> {reply, Amount, Account#account{balance = Balance - Amount}};
        false -> {reply, Balance, #account{}}
    end;
handle_call(_, _From, Error) ->
    {reply, {error, Error}, Error}.

%% @private
handle_cast({deposit, Amount}, Account) when Amount =< 0 -> {noreply, Account};
handle_cast({deposit, Amount}, Account = #account{balance = Balance}) ->
    {noreply, Account#account{balance = Balance + Amount}};
handle_cast(_, Error) ->
    {noreply, Error}.

%% @private
handle_info(timeout, State) -> {noreply, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.
