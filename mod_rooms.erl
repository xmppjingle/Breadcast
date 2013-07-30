-module(mod_rooms).

-behaviour(gen_server).

-define(INFO_MSG(M, P), lager:info(M, P)).
-define(ERROR_MSG(M, P), lager:error(M, P)).

%% API
-export([create/2, info/1, join/2, message/3, leave/2]).

%% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(room, {
	id = now() :: erlang:timestamp(),
	owner :: string(),
	joined = [] :: list(string()),
	title :: string(),
	timestamp = now() :: erlang:timestamp()
}).

-record(user, {
	jid :: string(),
	mute = now() :: erlang:timestamp(),
	timestamp = now() :: erlang:timestamp()
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

%% Module Functions

create(#user{}=Owner, Title) ->
	Room = #room{owner=Owner, title=Title},
	write_room(Room),
	broadcast_event(Room, {created, Room, Owner}).

join(#room{joined=Joined}=R, #user{}=User) ->
	Room=R#room{joined=[User|Joined]},
	write_room(Room),
	broadcast_event(R, {joined, Room, User}).

message(#room{}=R, #user{}=User, Body) ->
	broadcast_event(R, {message, R, User, Body}).

leave(#room{joined=Joined}=Room, #user{jid=JID}=User) ->
	lists:filter(fun(X) ->
					case X of
						JID ->
							write_room(Room),
							broadcast_event(Room, {left, Room, User}),
							false;
						_ ->
							true
					end
				end, Joined).

info(Id) ->
	read_room(Id).

%$ Event Functions

broadcast_event(#room{joined=Joined}, Event) ->
	broadcast_event(Joined, Event);
broadcast_event([], _Event) ->
	ok;
broadcast_event([#user{mute=Mute}=User|J], Event)->
	MuteDelta = timer:now_diff(now(), Mute),
	send_event(User, MuteDelta, Event),
	broadcast_event(J, Event).

send_event(#user{jid=_Jid}, MuteDelta, _Event) when MuteDelta > 0  ->
	%% Dispatch
	ok;
send_event(_,_,_) ->
	%% Muted User
	error.

%% Persistance Functions

prepare_tables() ->
	mnesia:start(),
    mnesia:create_table(room,
            [{ram_copies, [node()]},
             {type, set},
             {attributes, record_info(fields, room)}]).

read_room(Id) ->
	case mnesia:dirty_read(room, Id) of
		[] ->
			undefined;
		[R|_] ->
			R;
		_ ->
			error
	end.

write_room(#room{}=R) ->
	mnesia:dirty_write(R),
	R;
write_room(_) ->
	error.
