-module(ants).

-export([main/1]).
-mode(compile).

%% -compile(export_all).

readline_iterator(File) ->
    fun() ->
        case file:read_line(File) of
            {ok, Line} -> {ok, Line, readline_iterator(File)};
            eof -> done;
            {error, _Reason} = Err -> Err
        end
    end.

main([Filename]) ->
    {ok, File} = file:open(Filename, [read, {encoding,unicode}]),
    {ok, DotsP} = re:compile(":", [unicode]),
    {ok, RuleP} = re:compile("(?<n>\\d+)(?<rule>.{3})(?<l>.?)", [unicode]),
    foreach(
        fun(Line) ->
            %% io:format("----------------------------\n"),
            Bin = unicode:characters_to_binary(Line),
            [Data, RuleStr] = re:split(Bin, DotsP, [{return, binary}]),
            {match, [PositionBin, Rule, Letter]} =
                 re:run(RuleStr, RuleP, [{capture, [n,rule,l], binary}]),

            Position = binary_to_integer(PositionBin),
            DataList = [<<Ch/utf8>>||<<Ch/utf8>> <= Data],
            LettersI1 = from_list(DataList),
            LettersI2 = apply_rule({Position, Rule, Letter}, LettersI1),
            %% p(LettersI1),
            %% io:format("~p ~s ~s\n", [Position, Rule, Letter]),
            %% p(LettersI2),
            InfinitX = fun Loop() -> {ok, <<"Х"/utf8>>, Loop} end,
            InfLettersI1 = append(LettersI1, InfinitX),
            InfLettersI2 = append(LettersI2, InfinitX),

            N = 2,
            BothI = zip(by(N, InfLettersI1), by(N, InfLettersI2)),
            ResI = dropwhen(
                fun({[<<"Х"/utf8>>, <<"Х"/utf8>>], [<<"Х"/utf8>>,<<"Х"/utf8>>]}) -> true;
                   (_) -> false
                end, BothI),
            %% p2(ResI),

            {ok, Res} = fold(fun({A, B}, S) -> metrics(A,B) + S end, 0, ResI),
            io:format("~p\n", [Res])
        end,
        readline_iterator(File)).

apply_rule({Pos, <<"удл"/utf8>>, _}, I) ->
    drop(Pos, I);
apply_rule({Pos, <<"изм"/utf8>>, L}, I) ->
    replace(Pos, L, I);
apply_rule({Pos, <<"вст"/utf8>>, L}, I) ->
    add(Pos+1, L, I).

metrics(A, B) ->
    case metrics_(A, B) of
        {ok, V} -> V;
        error ->
            case metrics_(B, A) of
                {ok, V} -> V;
                error -> 100
            end
    end.

metrics_(A, A) -> {ok, 0};
metrics_([<<"К"/utf8>>, <<"Л"/utf8>>], [<<"К"/utf8>>, <<"Х"/utf8>>]) -> {ok, 0};
metrics_([<<"Л"/utf8>>, <<"К"/utf8>>], [<<"Л"/utf8>>, <<"П"/utf8>>]) -> {ok, 0};
metrics_([<<"Л"/utf8>>, <<"Л"/utf8>>], [<<"Л"/utf8>>, <<"Х"/utf8>>]) -> {ok, 0};
metrics_([<<"П"/utf8>>, <<"К"/utf8>>], [<<"П"/utf8>>, <<"П"/utf8>>]) -> {ok, 0};

metrics_([<<"Х"/utf8>>, <<"К"/utf8>>], [<<"Х"/utf8>>, <<"Л"/utf8>>]) -> {ok, 5};
metrics_([<<"П"/utf8>>, <<"К"/utf8>>], [<<"К"/utf8>>, <<"П"/utf8>>]) -> {ok, 5};
metrics_([<<"П"/utf8>>, <<"П"/utf8>>], [<<"К"/utf8>>, <<"П"/utf8>>]) -> {ok, 5};
metrics_([<<"К"/utf8>>, <<"К"/utf8>>], [<<"Л"/utf8>>, <<"Л"/utf8>>]) -> {ok, 5};
metrics_([<<"К"/utf8>>, <<"К"/utf8>>], [<<"Л"/utf8>>, <<"Х"/utf8>>]) -> {ok, 5};

metrics_([<<"П"/utf8>>, <<"Л"/utf8>>], [<<"П"/utf8>>, <<"Х"/utf8>>]) -> {ok, 25};
metrics_([<<"П"/utf8>>, <<"Л"/utf8>>], [<<"К"/utf8>>, <<"Л"/utf8>>]) -> {ok, 25};
metrics_([<<"П"/utf8>>, <<"Л"/utf8>>], [<<"К"/utf8>>, <<"Х"/utf8>>]) -> {ok, 25};
metrics_([<<"П"/utf8>>, <<"Х"/utf8>>], [<<"К"/utf8>>, <<"Л"/utf8>>]) -> {ok, 25};
metrics_([<<"П"/utf8>>, <<"Х"/utf8>>], [<<"К"/utf8>>, <<"Х"/utf8>>]) -> {ok, 25};
metrics_([<<"Х"/utf8>>, <<"П"/utf8>>], [<<"П"/utf8>>, <<"Х"/utf8>>]) -> {ok, 25};
metrics_([<<"Х"/utf8>>, <<"П"/utf8>>], [<<"П"/utf8>>, <<"Л"/utf8>>]) -> {ok, 25};
metrics_([<<"Х"/utf8>>, <<"П"/utf8>>], [<<"К"/utf8>>, <<"Л"/utf8>>]) -> {ok, 25};
metrics_([<<"Х"/utf8>>, <<"П"/utf8>>], [<<"К"/utf8>>, <<"Х"/utf8>>]) -> {ok, 25};
metrics_(_, _) -> error.

%% === Iterator ================================================================

empty() -> fun() -> done end.

from_list(List) ->
    Loop =
        fun([]) -> done;
           ([H|T]) -> {ok, H, from_list(T)}
        end,
    fun() -> Loop(List) end.

take(N, Iterator) when N > 0 ->
    take_(N, [], Iterator).
take_(0, Acc, I) -> {ok, lists:reverse(Acc), I};
take_(C, Acc, I) ->
    case I() of
        {ok, Data, Next} -> take_(C-1, [Data|Acc], Next);
        done -> {ok, lists:reverse(Acc), empty()};
        {error, _Reason} = Err -> Err
    end.

fold(Fun, State, Iterator) ->
    case Iterator() of
        {ok, Data, Next} ->
            fold(Fun, Fun(Data, State), Next);
        done -> {ok, State};
        {error, _Reason} = Err -> Err
    end.

foreach(Fun, Iterator) ->
    case Iterator() of
        {ok, Data, Next} ->
            _ = Fun(Data),
            foreach(Fun, Next);
        done -> ok;
        {error, _Reason} = Err -> Err
    end.

append(Iterator1, Iterator2) ->
    fun() ->
        case Iterator1() of
            {ok, Data, Next} -> {ok, Data, append(Next, Iterator2)};
            done -> Iterator2();
            {error, _Reason} = Err -> Err
        end
    end.

dropwhen(Fun, Iterator) ->
    fun() ->
        case Iterator() of
            {ok, Data, Next} ->
                case Fun(Data) of
                    true -> done;
                    false -> {ok, Data, dropwhen(Fun, Next)}
                end;
            done -> done;
            {error, _Reason} = Err -> Err
        end
    end.

zip(I1, I2) ->
    fun() ->
        {ok, D1, Next1} = I1(),
        {ok, D2, Next2} = I2(),
        {ok, {D1, D2}, zip(Next1, Next2)}
    end.

by(N, I) ->
    fun() ->
        case take(N, I) of
            {ok, Data, Next} ->
                {ok, Data, by(N, Next)};
            Other -> Other
        end
    end.

drop(N, I) ->
    fun() ->
        case I() of
            {ok, Data, Next} ->
                case N =:= 1 of
                    true -> Next();
                    false -> {ok, Data, drop(N-1, Next)}
                end;
            Other -> Other
        end
    end.

replace(N, Value, I) ->
    fun() ->
        case I() of
            {ok, Data, Next} ->
                case N =:= 1 of
                    true -> {ok, Value, Next};
                    false -> {ok, Data, replace(N-1, Value, Next)}
                end;
            Other -> Other
        end
    end.

add(1, Value, I) ->
    fun() -> {ok, Value, I} end;
add(N, Value, I) ->
    fun() ->
        case I() of
            {ok, Data, Next} ->
                {ok, Data, add(N-1, Value, Next)};
            Other -> Other
        end
    end.

%% p(I) ->
%%     foreach(fun(V) -> io:format("~s", [V]) end, I),
%%     io:format("\n").

%% p2(I) ->
%%     foreach(fun({[A,B],[C,D]}) -> io:format("(~s~s) <> (~s~s): ~p\n", [A,B,C,D, metrics([A,B], [C,D])]) end, I),
%%     io:format("\n").
