-module(spellchecker_bot).

%% Synopsis:
%% spellchecker_bot:start_link(chatroom:local_chatroom(), "/usr/share/dict/danish").


%% Public API
-export([start_link/2]).

%% For spawning
-export([init/2]).

-define(PREFIX, "Spelling nazi says:").

start_link(ChatRoom, DictFilename) when is_pid(ChatRoom), is_list(DictFilename) ->
    proc_lib:start_link(?MODULE, init, [ChatRoom, DictFilename]).

init(ChatRoom, DictFilename) ->
    DictTable = ets:new(dictionary, [protected]),
    {ok,BinDict} = file:read_file(DictFilename),
    BinWords = binary:split(BinDict, <<"\n">>, [global,trim]),
    lists:foreach(fun(W) ->
                          Key = string:to_lower(binary_to_list(W)),
                          ets:insert(DictTable, {Key}) end,
                  BinWords),
    proc_lib:init_ack(self()),
    ChatRoom ! {join, self()},
    loop(ChatRoom, DictTable).

loop(ChatRoom, DictTable) ->
    receive
        ?PREFIX ++ _ ->
            ok; % Ignore messages from self.
        M ->
            case is_string(M) of
                true ->
                    spellcheck(M, DictTable, ChatRoom);
                false ->
                    ok
            end
    end,
    loop(ChatRoom, DictTable).

spellcheck(String, DictTable, ChatRoom) ->
    case bad_words_in_string(String, DictTable) of
        [] ->
            ok;
        [BadWord|_] ->
            Msg = lists:flatten(io_lib:format("~s \"~s\" is not a word.",
                                              [?PREFIX, BadWord])),
            ChatRoom ! Msg
        end.

bad_words_in_string(String, DictTable) ->
    case re:run(String, "\\w+", [global,{capture,all,list}]) of
        {match, Words} ->
            [W || [W] <- Words, not word_exists(DictTable,W)];
        nomatch ->
            []
    end.

word_exists(DictTable,Word) ->
    LCWord = string:to_lower(Word),
    ets:member(DictTable, LCWord).



is_string([]) -> true;
is_string([H|T]) ->
    is_integer(H) andalso
        H>=0 andalso
        H=<16#FFFF andalso
        is_string(T);
is_string(_) -> false.
