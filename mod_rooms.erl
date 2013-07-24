-module(mod_rooms).

-behaviour(gen_server).

-define(INFO_MSG(M, P), lager:info(M, P)).
-define(ERROR_MSG(M, P), lager:error(M, P)).

%% API
-export([create/2, info/1]).

%% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(room, {
	id = now() :: erlang:timestamp(),
	owner :: string(),
	joined = [] :: list(string()),
	invited = [] :: list(string()),
	title :: string(),
	created = now() :: erlang:timestamp()
}).

-record(state, {rooms}).

start_link(State) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).
 
init(State) ->
    %% Some configuration operation here (e.g. handle ets/mnesia table)
    case prepare_tables() of
    	{Rooms, ok} -> 
    		{ok, #state{rooms=Rooms}};
    	_Error ->
    		{error, _Error}
    end.

handle_call(_Request, _From, State) ->
 	Reply = ok,
 	{reply, Reply, State}.

handle_cast(_, State) ->
    %% Do your configuration here
    {noreply, State}.

handle_info(_, State) ->
    %% Do your configuration here
    {noreply, State}.

terminate(_Reason, _State) ->
	ok.
 
code_change(_OldVsn, State, _Extra) ->
 	{ok, State}.

%% Tables

prepare_tables() ->
	mnesia:start(),
    mnesia:create_table(room,
            [{ram_copies, [node()]},
             {type, set},
             {attributes, record_info(fields, room)}]).

%% Module Functions

create(Owner, Title) ->
	Room = #room{owner=Owner, title=Title},
	mnesia:dirty_write(Room).

invite(#room{joined=Joined, invited=Invited}=R, User) ->
	R#room{invited=[User|Invited]}.

info(Id) ->
	case mnesia:dirty_read(room, Id) of
		[] ->
			undefined;
		[R|_] ->
			R;
		_ ->
			error
	end.
