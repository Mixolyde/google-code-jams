%%%-------------------------------------------------------------------
%%% File    : codejamwelcome.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Google Code Jam Template Module for doing the generic
%%%   file i/o that seems common to google code jam problems.
%%%   To use this template, rename the file, modify the module declaration,
%%%   LINESPERCASE, print_output_device, solvecase, and parsecase to
%%%   solve the particular problem
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

%% erlang shell commands to run:
%% cd ("C:/Users/bwilliams/Dropbox/dev/erl/codejam").
%% c(codejamtemplate).

-module(codejamwelcome).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

%% Number of lines to split off the input lines per case
-define(LINESPERCASE, 3).

%% Number of result values to output on a case line
-define(RESULTSPERLINE, 1).

% Quick start method calls
codejamsmalla() -> codejam("A-small-practice.in").
codejamlargea() -> codejam("A-large-practice.in").
codejamsmallb() -> codejam("B-small-practice.in").
codejamlargeb() -> codejam("B-large-practice.in").
codejamsmallc() -> codejam("C-small-practice.in").
codejamlargec() -> codejam("C-large-practice.in").
codejamsample() -> codejam("sample.in").

%%-----------------------------------------------------------------------------
%% Function: codejame/1
%% Purpose: Solve the codejam problem in the provided input file
%% Args: InFilename is the path/name of the input file
%% Returns: ok or {error, Reason} if it crashes
%%-----------------------------------------------------------------------------
codejam(InFilename) ->
  io:format("DEBUG: Opening ~s for input.~n", [InFilename]),
  % substring out the "main" part of the filename (part before the .in)
  MainFilename = string:substr(InFilename, 1, string:str(InFilename, ".in") - 1),
  % create an outputfile name like main_name.out
  OutFilename = MainFilename ++ ".out",

  {ok, InD} = file:open(InFilename, [read]),

  % read all the lines of the file into a list of lines in memory
  % split the first line off, as it is almost always the number of cases in the file
  [NumCaseString | Caselines] = get_all_lines(InD, []),
  [NumCases] = parse_ints(NumCaseString),

  % send all of the data lines to the solver
  Results = solvecases(Caselines),

  % assert that we have the right number of results
  NumCases = length(Results),
  % debug result print
  io:format("DEBUG: Results: ~w~n", [Results]),

  % send results to the output file and exit
  print_output(OutFilename, Results),
  ok.

%%-----------------------------------------------------------------------------
%% Function: get_all_lines/2
%% Purpose: Returns all the lines of an input file as a list of lines
%% Args: Device is an IO Device handle, Accum is the accumulator that recursive
%%   calls to the method use to store the lines, start with an empty list
%% Returns: List of strings or {error, Reason} if it crashes
%% NOTE: By default this method closes the IO Device when it's done reading
%%-----------------------------------------------------------------------------
get_all_lines(Device, Accum) ->
  case io:get_line(Device, "") of
    % hit end of file, close the device and return
    eof  -> file:close(Device), lists:reverse(Accum);
    % strip the line return off the end
    Line -> get_all_lines(Device, [string:strip(Line, both, $\n)|Accum])
  end.

%%-----------------------------------------------------------------------------
%% Function: print_output/2
%% Purpose: Formats and prints the result list to the output file device
%% Args: Device is an IO Device handle, Accum is the accumulator that recursive
%%   calls to the method use to store the lines, start with an empty list
%% Returns: List of strings or {error, Reason} if it crashes
%% NOTE: By default this method closes the IO Device when it's done reading
%%-----------------------------------------------------------------------------
print_output(OutFilename, Results) ->
  io:format("DEBUG: Opening ~s for output.~n", [OutFilename]),
  {ok, OutD} = file:open(OutFilename, [write]),
  ok = print_output_device(OutD, Results, 1),
  file:close(OutD).

%% print results out one case at a time, return when out of results to print
print_output_device(_OutD, [], _Count) -> ok;  %termination case, return ok status
print_output_device(OutD, [Case | Rest], Count) ->
  OutputList = [Count] ++ tuple_to_list(Case),
  % Format = "Case #~w:" ++ string:copies(" ~w", ?RESULTSPERLINE) ++ "~n",
  Format = "Case #~w: ~4..0w~n",
  io:format(Format, OutputList),
  io:format(OutD, Format, OutputList),
  print_output_device(OutD, Rest, Count + 1).


%%-----------------------------------------------------------------------------
%% Function: solvecases/1
%% Purpose: Splits the input lines into input groups and solves the problem
%% Args: List of input lines to solve
%% Returns: List of results for each case or {error, Reason} if it crashes
%%-----------------------------------------------------------------------------
solvecases(Caselines) ->
  % start the recursive solver at Case #1 with empty result accumulator
  solvecase(Caselines, []).

solvecase([], Accum) -> lists:reverse(Accum); %termination case, reverse the results and return
solvecase([FirstLine | RestLines], Accum) ->
  % io:format("DEBUG: Solving Case #~w~n", [NumCases]),

  % brute force breadth-first search to solve a case
  Combinations = search("welcome to code jam", FirstLine),
  io:format("Got ~w combinations for line: ~p~n", [Combinations, FirstLine]),

  % push the result onto the accumulator and recurse
  solvecase(RestLines, [{Combinations} | Accum]).

%%-----------------------------------------------------------------------------
%% Function: parsecase/1
%% Purpose: Takes a list of case lines and parses them into the data variables
%%   needed to solve the problem
%% Args: List of input lines to parse
%% Returns: Tuple of values for solving or an error
%%-----------------------------------------------------------------------------
parsecase([CreditLine, ItemCountLine, ItemLine]) ->
  {Credit, []} = string:to_integer(CreditLine),
  {ItemCount, []} = string:to_integer(ItemCountLine),
  Items = parse_ints(ItemLine),
  ItemCount = length(Items),
  {IndexedItems, _ReturnIndex} = lists:mapfoldl(fun(Item, Index) -> {{Index, Item}, Index + 1} end, 1, Items),
  % io:format("Indexed list: ~w~n", [IndexedItems]),
  FilteredItems = lists:filter(fun({_Index, Item}) -> Item < Credit end, IndexedItems),
  SortedFilteredItems = lists:sort(fun({_Index, A}, {_OtherIndex, B}) -> A =< B end, FilteredItems),
  {Credit, SortedFilteredItems}.

%% parses a line into ints
parse_ints(Line) ->
  Tokens = string:tokens(Line, " "),
  lists:map(fun(Token) -> parse_int(Token) end, Tokens).

%% parse a line with single value into an int
parse_int(Line) ->
  {Result, _Rest} = string:to_integer(Line),
  Result.

%% Case solving algorithm
%% extraordinarily naive search. get match, count possible matches from here plus all possible matches after this one
naivesearch([], _Line) ->
  % finished going through our search string, so that's a success
  1;
naivesearch(_SearchString, []) ->
  % ran out of line to search through, not a match
  0;
naivesearch([SearchChar | RestSearch], [SearchChar | RestLine]) ->
    % the first two charachters of our inputs match, so keep search through this input for possibilities
    % and keep searching with current search string for other combinations
    % io:format("SearchChar matched: ~p~n", [SearchChar]),
    naivesearch([SearchChar | RestSearch], RestLine) + naivesearch(RestSearch, RestLine);
naivesearch([SearchChar | RestSearch], [_NotSearchChar | RestLine]) ->
    % the first two charachters of our inputs do not match, so continue search at next char in the line
    % io:format("SearchChar ~p did not match ~p~n", [SearchChar, _NotSearchChar]),
    naivesearch([SearchChar | RestSearch], RestLine).

% attempt to make naive search a little smarter and cut out of searches with no hope early
search([], _Line) ->
  % finished going through our search string, so that's a success
  1;
search(_SearchString, []) ->
  % ran out of line to search through, not a match
  0;
search([SearchChar | RestSearch], [SearchChar | RestLine]) ->
    % the first two characters of our inputs match, so keep search through this input for possibilities
    % and keep searching with current search string for other combinations
    % io:format("SearchChar matched: ~c~n", [SearchChar]),
    % depth first search the rest of the search space
    RestCharsCount = search(RestSearch, RestLine),
    % if there are 0 matches of the rest of the chars, we know there are no more full matches
    % with this char either
    case RestCharsCount > 0 of
      true -> RestCharsCount + search([SearchChar | RestSearch], RestLine);
      false ->
        RestCharsCount
    end;
%if the first two charachters don't match, check for line lengths
search(FullSearch = [_SearchChar | _RestSearch], [_NotSearchChar | RestLine])
  when length(FullSearch) =< length(RestLine) ->
    % the first two characters of our inputs do not match, so continue search at next char in the line
    % io:format("SearchChar ~p did not match ~p~n", [SearchChar, _NotSearchChar]),

    % do a quick pair of O(n) character count calculations to see if it makes sense to keep going
    SearchCharacterCounts = character_count(FullSearch),
    LineCharacterCounts   = character_count(RestLine),

    % one more O(n) op to see if we should keep going or fail out quickly
    case should_continue(SearchCharacterCounts, LineCharacterCounts) of
      true -> search(FullSearch, RestLine);
      false -> 0
    end;
search(_FullSearch, _FullLine) ->
  % io:format("Search is longer than the rest of the line: ~p to ~p~n", [_FullSearch, _FullLine]),
  0.

% start counter with empty accumulator
character_count(Characters) -> character_count(Characters, []).
character_count([], Accum) -> Accum;
character_count([Char | Rest], Accum) ->
  case lists:keysearch(Char, 1, Accum) of
    {value, {Char, Count}} ->
      % search for rest with an updated count
      character_count(Rest, lists:keyreplace(Char, 1, Accum, {Char, Count+1}) );
    false ->
      % add a new count tuple for this new char
      character_count(Rest, [{Char, 1} | Accum])
  end.

should_continue([], _LineCounts) -> true;
should_continue([{SearchChar, SearchCount} | RestSearch], LineCounts) ->
  % search line counts for char and see if there are enough left {$a, 3}
  case lists:keysearch(SearchChar, 1, LineCounts) of
    {value, {SearchChar, LineCount}} when SearchCount > LineCount ->
      % required search count is hiring than the line we're searching in so fail fast
      false;
    {value, {SearchChar, _LineCount}} ->
      % keep searching
      should_continue(RestSearch, LineCounts);
    false ->
      % char not found at all in the rest of the line, fail
      false
  end.

unit_test() ->
  %1 = search("why", "why"),
  0 = search("abcde", "zde"),
  %1 = search("", ""),
  %1 = search("", "abc"),
  %0 = search("abc", ""),
  3 = search("a", "aaa"),
  3 = search("ab", "aaab"),
  2 = search("a b", "ab bb"),
  9 = search("a b", "aaa bbb"),
  18 = search("a b", "aaa  bbb"),
  6 = search("ab", "ababab"),
  [{$a, 1}, {$b, 1}] = character_count("ba"),
  [{$a, 2}, {$b, 3}] = character_count("bbaba"),
  true = should_continue([{$a, 2}, {$b, 3}], [{$a, 2}, {$b, 3}]),
  true = should_continue([{$a, 2}, {$b, 3}], [{$a, 4}, {$b, 5}]),
  false = should_continue([{$a, 2}, {$b, 3}], [{$b, 3}]),
  false = should_continue([{$a, 2}, {$b, 3}], [{$a, 4}, {$b, 1}]),
  false = should_continue([{$a, 2}, {$b, 3}], [{$a, 1}, {$b, 3}]),

    0 = search("welcome to code jam", "so dqmweawewwwwwewwweoeeecweeeeeeljeeem llleclljllcclccllcocdcccoocoeomc moommmojmm oom ommee e eeeeeceem     ee cj ttwetoe t  oo t  ttoowotootto oo  e oo do   ocl voc c ce cdooococodcmocoeodo ododddoodededddddedtecee de eeem j ee     jr jt jm jjcjjjjjjajoaaaaaaaade"),
  % 0 = search("welcome to code jam", "so dqmweawewwwwwewwweoeeecweeeeeeljeeem llleclljllcclccllcocdcccoocoeomc moommmojmm oom ommee e eeeeeceem     ee cj ttwetoe t  oo t  ttoowotootto oo  e oo do   ocl voc c ce cdooococodcmocoeodo ododddoodededddddedtecee de eeem j ee     jr jt jm jjcjjjjjjajoaaaaaaaademmaajmtmmmmmmmdm ommh ei"),
  ok.

