%%%-------------------------------------------------------------------
%%% File    : africa2010storecredit.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Google Code Jam Practice Entry
%%%
%%% Created :  14 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

%% cd ("C:/Users/bwilliams/Dropbox/dev/erl/codejam").
%% c(africa2010storecredit).

-module(africa2010storecredit).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

storecredit(InFilename) ->
  io:format("Opening ~s for input.~n", [InFilename]),
  MainFilename = string:substr(InFilename, 1, string:str(InFilename, ".in") - 1),
  OutFilename = MainFilename ++ ".out",
  io:format("Opening ~s for output.~n", [OutFilename]),
  {ok, InD} = file:open(InFilename, [read]),
  [NumCaseString | Caselines] = get_all_lines(InD, []),
  {NumCases, []} = string:to_integer(NumCaseString),
  {ok, OutD} = file:open(OutFilename, [write]),
  Results = solvecases(Caselines),
  NumCases = length(Results),
  io:format("Results: ~w~n", [Results]),
  print_output(OutD, Results),
  file:close(InD),
  file:close(OutD),
  ok.
  
solvecases(Caselines) ->
  solvecase(Caselines, 1, []).

solvecase([], _Count, Accum) -> lists:reverse(Accum);
solvecase(Lines, Count, Accum) ->
  % io:format("Solving Case #~w~n", [NumCases]),
  % split the first 3 lines off the input
  {CaseLines, RestLines} = lists:split(3, Lines),
  {Credit, Items} = parsecase(CaseLines),
  % io:format("Have ~w to spend on ~w items: ~w~n", [Credit, length(Items), Items]),
  
  % now that we've parsed, filtered and sorted the data file, we can actually solve the problem!
  
  % brute force breadth-first search
  {success, {FirstIndex, FirstItem}, {SecondIndex, SecondItem}} = bfs(Credit, Items),
  Credit = FirstItem + SecondItem,
  %add results of this case to accumulator and recurse
  if FirstIndex < SecondIndex ->
      %io:format("Case #~w: ~w ~w~n", [Count, FirstIndex, SecondIndex]),
      solvecase(RestLines, Count+1, [{Count, FirstIndex, SecondIndex} | Accum]);
    true ->
      %io:format("Case #~w: ~w ~w~n", [Count, SecondIndex, FirstIndex]),
      solvecase(RestLines, Count+1, [{Count, SecondIndex, FirstIndex} | Accum])
  end.
  
bfs(_Credit, [_Item]) ->
  %only one item left, fail
  {failure};
bfs(Credit, [{FirstIndex, FirstItem} | Rest]) ->
  Match = Credit - FirstItem,
  % we know what the match would be if it's paired with the first item
  % so search for it
  case lists:keyfind(Match, 2, Rest) of 
    {Index, Match} ->
      {success, {FirstIndex, FirstItem}, {Index, Match}};
    _Else ->
      %look in Rest for a match
      bfs(Credit, Rest)
  end.

get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), lists:reverse(Accum);
        Line -> get_all_lines(Device, [string:strip(Line, both, $\n)|Accum])
    end.
    
parsecase([CreditLine, ItemCountLine, ItemLine]) ->
  {Credit, []} = string:to_integer(CreditLine),
  {ItemCount, []} = string:to_integer(ItemCountLine),
  ItemTokens = string:tokens(ItemLine, " "),
  Items = lists:map(fun(ItemToken) -> {Integer, []} = string:to_integer(ItemToken), Integer end, ItemTokens),
  ItemCount = length(Items),
  {IndexedItems, _ReturnIndex} = lists:mapfoldl(fun(Item, Index) -> {{Index, Item}, Index + 1} end, 1, Items),
  % io:format("Indexed list: ~w~n", [IndexedItems]),
  FilteredItems = lists:filter(fun({_Index, Item}) -> Item < Credit end, IndexedItems),
  SortedFilteredItems = lists:sort(fun({_Index, A}, {_OtherIndex, B}) -> A =< B end, FilteredItems),
  {Credit, SortedFilteredItems}.

print_output(_OutD, []) -> ok;
print_output(OutD, [{Count, FirstIndex, SecondIndex} | Rest]) ->
  io:format("Case #~w: ~w ~w~n", [Count, FirstIndex, SecondIndex]),
  io:format(OutD, "Case #~w: ~w ~w~n", [Count, FirstIndex, SecondIndex]),
  print_output(OutD, Rest).
