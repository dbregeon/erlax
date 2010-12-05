%%% -------------------------------------------------------------------
%%% Author  : dbregeon
%%% Description :
%%%
%%% Created : 16 avr. 2010
%%% -------------------------------------------------------------------
-module(couchdb_access).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([save/3, load/3, new_identifier/2]).


%% ====================================================================
%% External functions
%% ====================================================================
save(Url, {Identifier, Revision , Status, Document}, RequestFunction) ->
	save_document(document_url(Url, Identifier),
				  apply_identification(Identifier, Revision, Status, Document),
				  RequestFunction);
save(_,_,_) -> {error, invalid_arguments}.

load(Url, Identifier, RequestFunction) ->
	load_document(document_url(Url, Identifier), RequestFunction).

new_identifier(Url, RequestFunction) -> 
	{ok, {_Status, _Headers, Body}} = RequestFunction(Url),
	{ok, Response, []} = rfc4627:decode(Body),
	case rfc4627:get_field(Response, "uuids") of
		{ok, [BinaryId]} -> binary_to_atom(BinaryId, latin1);
		_ 	 ->  {error, uuids_missing_from_response}
	end.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
document_url(Url, Identifier) -> lists:flatten(lists:append([Url, "/", atom_to_list(Identifier)])).

apply_identification([], [], Status, Document) -> set_status(Document, Status);
apply_identification(Identifier, Revision, Status, Document) -> 
	set_rev(set_id(set_status(Document, Status), Identifier), Revision).

remove_identification(Document) -> 
	remove_status(remove_rev(remove_id(Document))).

save_document(Url, Document, RequestFunction) -> 
	EncodedDocument = rfc4627:encode(Document),
	case RequestFunction(Url, EncodedDocument) of
		{ok, {_Status, _Headers, Body}} -> save_succeeded(Document, Body);
		Error							-> save_failed(Document, Error)
	end.

load_document(Url, RequestFunction) ->
	{ok, {_Status, _Headers, Body}} = RequestFunction(Url),
	{ok, Document, []} = rfc4627:decode(Body),
	{ok, id(Document), rev(Document), status(Document), remove_identification(Document)}.

save_succeeded(Document, EncodedResponse) ->
	{ok, Response, []} = rfc4627:decode(EncodedResponse),
	case {Id = id_from_response(Response), Rev = rev_from_response(Response)} of
		{[],_}	-> save_failed(Document, Response);
		{_,[]}	-> save_failed(Document, Response);
		_		-> {ok, Id, Rev}
	end.

save_failed(Document, Reason) -> {error, {save_failed, Reason, Document}}.

id_from_response(JSONResponse) ->
	case rfc4627:get_field(JSONResponse, "id") of
		{ok, BinaryId} 	-> binary_to_atom(BinaryId, latin1);
		_				-> []
	end.

rev_from_response(JSONResponse) ->
	case rfc4627:get_field(JSONResponse, "rev") of
		{ok, BinaryRev}	-> BinaryRev;
		_				-> []
	end.

id(Document) ->
	case rfc4627:get_field(Document, "_id") of
		{ok, BinaryId} 	-> binary_to_atom(BinaryId, latin1);
		_				-> []
	end.

rev(Document) ->
	case rfc4627:get_field(Document, "_rev") of
		{ok, BinaryRev}	-> BinaryRev;
		_				-> []
	end.

status(Document) ->
	case rfc4627:get_field(Document, "status") of
		{ok, BinaryStatus}	-> binary_to_atom(BinaryStatus, latin1);
		_					-> []
	end.

set_id(Document, Identifier) -> rfc4627:set_field(Document, "_id", atom_to_binary(Identifier, latin1)).

set_rev(Document, []) -> Document;
set_rev(Document, Revision) -> rfc4627:set_field(Document, "_rev", Revision).

set_status(Document, Status) -> rfc4627:set_field(Document, "status", atom_to_binary(Status, latin1)).

remove_id({obj, Properties}) -> {obj, lists:keydelete("_id", 1, Properties)}.

remove_rev({obj, Properties}) -> {obj, lists:keydelete("_rev", 1, Properties)}.

remove_status({obj, Properties}) -> {obj, lists:keydelete("status", 1, Properties)}.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

id_from_response_should_return_the_id_value_of_a_json_object_test_()-> [
	?_assertEqual(test_id, id_from_response({obj, [{"id", <<"test_id">>}]}))
].

id_from_response_should_return_the_empty_list_when_id_is_not_defined_test_()-> [
	?_assertEqual([], id_from_response({obj, [{"rev", <<2>>}]}))
].

rev_from_response_should_return_the_rev_value_of_a_json_object_test_()-> [
	?_assertEqual(<<2>>, rev_from_response({obj, [{"rev", <<2>>}]}))
].

rev_from_response_should_return_the_empty_list_when_rev_is_not_defined_test_()-> [
	?_assertEqual([], rev_from_response({obj, [{"id", <<2>>}]}))
].

id_should_return_the_id_of_the_document_test_() -> [
	?_assertEqual(test_id, id({obj, [{"_id", <<"test_id">>}, {"_rev", <<4>>}]}))
].

id_should_return_the_empty_list_when_the_document_is_notfound_test_() -> [
	?_assertEqual([], id({obj, [{"error", "not_found"}, {"reason", "missing"}]}))
].

rev_should_return_the_rev_of_the_document_test_() -> [
	?_assertEqual(<<4>>, rev({obj, [{"_id", <<3>>}, {"_rev", <<4>>}]}))
].

rev_should_return_the_empty_list_when_the_document_is_notfound_test_() -> [
	?_assertEqual([], rev({obj, [{"error", "not_found"}, {"reason", "missing"}]}))
].

status_should_return_the_status_of_the_document_test_() -> [
	?_assertEqual(test_status, status({obj, [{"_id", <<3>>}, {"status", <<"test_status">>}]}))
].

status_should_return_the_empty_list_when_the_document_is_notfound_test_() -> [
	?_assertEqual([], status({obj, [{"error", "not_found"}, {"reason", "missing"}]}))
].

remove_id_should_remove_the_id_of_the_document_test_() -> [
	?_assertMatch({obj, [{"_rev", <<4>>}]}, remove_id({obj, [{"_id", <<3>>}, {"_rev", <<4>>}]}))
].

remove_id_should_not_change_the_document_when_no_id_is_found_test_() -> [
	?_assertMatch({obj, [{"_rev", <<4>>}]}, remove_id({obj, [{"_rev", <<4>>}]}))
].

remove_rev_should_remove_the_rev_of_the_document_test_() -> [
	?_assertEqual({obj, [{"_id", <<3>>}]}, remove_rev({obj, [{"_id", <<3>>}, {"_rev", <<4>>}]}))
].

remove_rev_should_not_change_the_document_when_no_rev_is_found_test_() -> [
	?_assertEqual({obj, [{"_id", <<3>>}]}, remove_rev({obj, [{"_id", <<3>>}]}))
].

remove_status_should_remove_the_rev_of_the_document_test_() -> [
	?_assertEqual({obj, [{"_id", <<3>>}]}, remove_status({obj, [{"_id", <<3>>}, {"status", <<"test_status">>}]}))
].

remove_status_should_not_change_the_document_when_no_status_is_found_test_() -> [
	?_assertEqual({obj, [{"_id", <<3>>}]}, remove_status({obj, [{"_id", <<3>>}]}))
].

set_id_should_change_the_id_of_the_document_test_() -> [
	?_assertMatch({obj, [{"_id", <<"test_id">>}, {"_rev", <<4>>}]}, set_id({obj, [{"_id", <<3>>}, {"_rev", <<4>>}]}, test_id))
].

set_rev_should_change_the_revision_of_the_document_test_() -> [
	?_assertMatch({obj, [{"_rev", <<1>>}, {"_id", <<3>>}]}, set_rev({obj, [{"_id", <<3>>}, {"_rev", <<4>>}]}, <<1>>))
].

set_rev_should_not_change_the_revision_of_the_document_when_revision_is_empty_test_() -> [
	?_assertMatch({obj, [{"_id", <<3>>}, {"_rev", <<4>>}]}, set_rev({obj, [{"_id", <<3>>}, {"_rev", <<4>>}]}, []))
].

set_status_should_change_the_revision_of_the_document_test_() -> [
	?_assertMatch({obj, [{"status", <<"test_status">>}, {"_id", <<3>>}]}, set_status({obj, [{"_id", <<3>>}]}, test_status))
].

save_should_return_the_id_and_revision_from_the_response_test_()-> [
	?_assertEqual(test_id, element(2, save_document(ok, {obj, [{"id", <<"test_id">>}, {"rev", <<4>>}]}, stub_http:save_request_function()))),
	?_assertEqual(<<2>>, element(3, save_document(ok, {obj, [{"id", <<3>>}, {"rev", <<4>>}]}, stub_http:save_request_function())))
].

save_should_return_an_error_when_response_is_erroneous_test_()-> [
	?_assertMatch({error, _}, save_document(no_id, {obj, [{"id", <<3>>}, {"rev", <<4>>}]}, stub_http:save_request_function())),
	?_assertMatch({error, _}, save_document(no_rev, {obj, [{"id", <<3>>}, {"rev", <<4>>}]}, stub_http:save_request_function())),
	?_assertMatch({error, _}, save_document(ok, {obj, [{"id", <<3>>}, {"rev", <<4>>}]}, stub_http:erroneous_save_request_function()))
].

apply_version_should_set_id_and_rev_on_the_object_test_()-> [
	?_assertMatch({obj, [{"_rev", <<2>>}, {"_id", <<"test_id">>}, {"status", <<"test_status">>}]}, apply_identification(test_id, <<2>>, test_status, {obj, [{"_id", <<"3">>}, {"_rev", <<4>>}]}))
].

document_url_should_append_identifier_to_base_url_test_() -> [
	?_assertEqual("http://localhost/test_name/test1", document_url("http://localhost/test_name", test1))					  
].

-endif.