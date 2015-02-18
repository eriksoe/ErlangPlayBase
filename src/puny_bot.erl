-module(puny_bot).
-compile(export_all).

%% Public API
-export([start_link/1]).

%% For spawning
-export([init/1]).

-define(COMMAND, "!puny ").
-define(SAY_PREFIX, "puny_bot: ").

-record(state, {active=true,
                dictionary,
                synonym_table}).

start_link(ChatRoom) when is_pid(ChatRoom) ->
    proc_lib:start_link(?MODULE, init, [ChatRoom]).

init(ChatRoom) ->
    proc_lib:init_ack(self()),
    setup(),
    ChatRoom ! {join, self()},
    loop(ChatRoom, #state{
           dictionary = build_dictionary("/usr/share/dict/words") % English dictionary
          }).

setup() ->
    inets:start().                              % Needed for the HTTP client.

loop(ChatRoom, State) ->
    receive
        ?COMMAND ++ "disable" ->
           State2 = State#state{active=false},
            ChatRoom ! ?SAY_PREFIX ++ "disabled";
        ?COMMAND ++ "enable" ->
            State2 = State#state{active=true},
            ChatRoom ! ?SAY_PREFIX ++ "enabled";
        ?COMMAND ++ "ping" ->
            State2 = State,
            ChatRoom ! ?SAY_PREFIX ++ "I am present and "++
                (case State#state.active of
                     true -> "enabled";
                     false -> "disabled"
                 end);
        ?SAY_PREFIX ++ _ ->
            % io:format("~s: Ignoring own remark.\n", [?MODULE]),
            State2 = State;
        _Msg when State#state.active == false ->
            State2 = State,
            ignore;
        Msg ->
            % io:format("~s: Trying to come up with a good reply to ~p\n", [?MODULE, Msg]),
            case puny_reply(Msg, State) of
                {ok, Reply} ->
                    % io:format("~s: Got reply: ~p\n", [?MODULE, Reply]),
                    ChatRoom ! ?SAY_PREFIX ++ Reply;
                _Err ->
                    % io:format("~s: Came up blank: ~p\n", [?MODULE, _Err]),
                    ok                          % No action
            end,
            State2 = State
    end,
    loop(ChatRoom, State2).


build_dictionary(DictFilename) ->
    DictTable = ets:new(dictionary, [protected]),
    {ok,BinDict} = file:read_file(DictFilename),
    BinWords = binary:split(BinDict, <<"\n">>, [global,trim]),
    lists:foreach(fun(W) ->
                          Key = normalize_word(W),
                          ets:insert(DictTable, {Key}) end,
                  BinWords),
    DictTable.

puny_reply(Msg, State) ->
    Words = words_of(Msg),
    RelevantWords = lists:filter(fun(W) -> is_relevant_english_word(W, State) end,
                                 Words),
    case RelevantWords of
        [] ->
            {error, no_words};
        _ ->
            Word = select_random(RelevantWords),
            case lookup_synonyms_for(Word) of
                {error, Reason} ->
                    {error, {synonym_failure_for, Word, Reason}};
                {ok, []} ->
                    {error, {no_synonyms_for, Word}};
                {ok, Synonyms} ->
                    Synonym = select_random(Synonyms),
                    {ok, "Your puny "++Word++" is no match for my mighty "++Synonym++"!"}
            end
    end.


words_of(MaybeLine) ->
    %% The message may not be a text string, so we guard the handling:
    try re:run(MaybeLine, "(\\w+)", [global, {capture,all_but_first,list}]) of
        {match, Matches} ->
            Words = lists:map(fun([Word]) -> normalize_word(Word) end, Matches),
            Words
    catch _:_ ->
            []
    end.

is_relevant_english_word(Word, #state{dictionary=Dict}) ->
    Word2 = normalize_word(Word),
    length(Word2) >= 3 andalso ets:member(Dict, Word2).

normalize_word(Word) ->
    string:to_lower(unicode:characters_to_list(Word, utf8)).

lookup_synonyms_for(Word) ->
    case httpc:request("http://www.thesaurus.com/browse/"++Word) of
        {ok, {{_,200,_}, _Headers, Body}} ->
            %% Find extent of first list
            case re:run(Body, "<div class=\"synonym-description\">\n<em class=\"txt\">noun</em>") of
                {match, [{NounSymHeaderStart,NounSymHeaderLen} | _]} ->
                    NounSynonymOffset = NounSymHeaderStart + NounSymHeaderLen,
                    case re:run(Body, "^<ul>", [multiline, global, {offset, NounSynonymOffset}]) of
                        {match, [[{Start,_}], [{End,_}] | _]} ->
                            SynonymListText = string:sub_string(Body, Start, End),
                            case re:run(SynonymListText, "^<span class=\"text\">(.*?)</span>", [multiline, global, {capture,all_but_first,list}]) of
                                {match, Matches} ->
                                    Synonyms = lists:map(fun([Synonym]) -> Synonym end,
                                                         Matches),
                                    {ok, Synonyms};
                                _ ->
                                    {error, no_synonym_items_found}
                            end;
                        _ ->
                            {error, no_ul_found}
                    end;
                _ ->
                    {error, no_noun_synonyms_found}
            end;
        _ ->
            {error, lookup_error}
    end.

select_random(List) ->
    N = length(List),
    K = random:uniform(N),
    lists:nth(K, List).

% <span class="text">grip</span>
