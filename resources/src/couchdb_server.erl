%%% -------------------------------------------------------------------
%%% Author  : dbregeon
%%% Description :
%%%
%%% Created : 15 avr. 2010
%%% -------------------------------------------------------------------
-module(couchdb_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {new_identifier_url, database_url, put_function, get_function}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link([Name | Args])->
	gen_server:start_link({local, Name}, couchdb_server, Args, []).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Url, Database]) ->
    {ok, #state{
				new_identifier_url = uuids_url(Url),
				database_url = database_url(Url, Database),
				put_function = put_request_function(),
				get_function = get_request_function()
				}
	}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({save, Args}, _From, State) ->
	{reply, couchdb_access:save(State#state.database_url, Args, State#state.put_function), State};
handle_call({load, Args}, _From, State) ->
	{reply, couchdb_access:load(State#state.database_url, Args, State#state.get_function), State};
handle_call({new_identifier, _Args}, _From, State) ->
	{reply, couchdb_access:new_identifier(State#state.new_identifier_url, State#state.get_function), State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Args, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
uuids_url(Url) -> string:concat(Url, "/_uuids").

database_url(Url, Database) -> string:concat(string:concat(Url, "/"), Database).

put_request_function() -> 
	fun(Url, Document) -> 
			http:request(put, {Url, [], "application/json", Document}, [], [])
	end.

get_request_function() -> 
	fun(Url) -> 
			http:request(Url) 
	end.

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

init_should_build_a_state_from_the_parameters_test_() ->
	Result = init(["url", "database"]),
	UuidsUrl = uuids_url("url"),
	DatabaseUrl = database_url("url", "database"),
	[
		?_assertMatch({ok, _State}, Result),
		?_assertMatch({_, #state{new_identifier_url = UuidsUrl}}, Result),
		?_assertMatch({_, #state{database_url = DatabaseUrl}}, Result)
	].

uuids_url_should_return_the_couchdb_url_to_request_a_uuid_test_() ->
	[
	 ?_assertMatch("url/_uuids", uuids_url("url"))
	].

database_url_should_append_the_database_name_to_the_url_test_() ->
	[
	 ?_assertMatch("url/database", database_url("url", "database"))
	].

put_request_function_should_return_a_two_arguments_function_test_() -> [].

get_request_function_should_return_a_one_argument_function_test_() -> [].

-endif.