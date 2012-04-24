%%%-------------------------------------------------------------------
%%% File    : codejamtemplate.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Google Code Jam Template Module for doing the generic
%%%   file i/o and solving structure that seems common to google code jam problems.
%%%
%%% To use this template, rename the file, modify the module declaration,
%%%   LINESPERCASE, RESULTSPERLINE, parsecase and search to
%%%   solve the particular problem
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

%% erlang shell commands to run:
%% cd ("C:/Users/bwilliams/Dropbox/dev/erl/codejam").
%% c(codejamtemplate).
%% codejamtemplate:codejamsmalla().

-module(codejamtemplate).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

%% Defines to modify per problem

%% Number of lines to split off the input lines per case
-define(LINESPERCASE, 3).

%% Number of result values to output on a case line
-define(RESULTSPERLINE, 2).

%% Quick start method calls
codejamsmalla() -> codejam("A-small-practice.in").
codejamlargea() -> codejam("A-large-practice.in").
codejamsmallb() -> codejam("B-small-practice.in").
codejamlargeb() -> codejam("B-large-practice.in").
codejamsmallc() -> codejam("C-small-practice.in").
codejamlargec() -> codejam("C-large-practice.in").
codejamsample() -> codejam("sample.in").
codejamtemplate() -> codejam("template.in").

%%-----------------------------------------------------------------------------
%% General codejam setup methods and utilities, not customized per
%%   problem.
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Function: codejam/1
%% Purpose: Solve the codejam problems in the provided input file
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
  Results = sequential_startsolve(Caselines, NumCases),
  % Results = parallel_startsolve(Caselines, NumCases),

  % assert that we have the right number of results
  NumCases = length(Results),

  % debug result print
  io:format("DEBUG: Results: ~w~n", [Results]),

  % send results to the output file and exit
  print_output(OutFilename, Results),
  ok.

%%-----------------------------------------------------------------------------
%% Function: get_all_lines/2
%% Purpose: Returns all the lines of an input file as a list of lines with the
%%   carriage return stripped
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
%% Purpose: Formats and prints the result list to the output file
%% Args: OutFilename is the file name to open, Results is a list of tuples that
%%  will be printed to the file, using the ?RESULTSPERLINE macro
%% Returns: ok or {error, Reason} if it crashes
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
  % Case 1: 1 2 3
  % Case 2: 4 5 6
  Format = "Case #~w:" ++ string:copies(" ~w", ?RESULTSPERLINE) ++ "~n",
  io:format(Format, OutputList),
  io:format(OutD, Format, OutputList),
  print_output_device(OutD, Rest, Count + 1).


%%-----------------------------------------------------------------------------
%% Function: sequential_startsolve/2
%% Purpose: Splits the input lines into input groups and solves the problems
%%   one at a time before continuing on to the next one
%% Args: List of input lines to solve, total number of cases to solve (for
%%   status printing)
%% Returns: List of results for each case or {error, Reason} if it crashes
%%-----------------------------------------------------------------------------
sequential_startsolve(Caselines, NumCases) ->
  % start the recursive solver at Case #1 with empty result accumulator
  sequential_solvecase(Caselines, NumCases, []).

sequential_solvecase([], NumCases, Accum) ->
  print_status(length(Accum), NumCases),     % print the last status
  lists:reverse(Accum);                      % termination case, reverse the results and return
sequential_solvecase(Lines, NumCases, Accum) ->
  print_status(length(Accum), NumCases),     % print the current status

  % split the next problem input lines off the input for this case
  {CaseLines, RestLines} = lists:split(?LINESPERCASE, Lines),

  % solve this case, customized for the problem
  Result = single_solvecase(CaseLines),

  % push the result onto the accumulator and solve the next case
  sequential_solvecase(RestLines, NumCases, [Result | Accum]).

%%-----------------------------------------------------------------------------
%% Function: parallel_startsolve/2
%% Purpose: Splits the input lines into input groups and spawns a worker process
%%   for each problem to solve individually and send their result back to a
%%   collecting process
%% Args: List of input lines to solve, total number of cases to solve (for
%%   the receive loop to know when it's done)
%% Returns: List of results for each case or {error, Reason} if it crashes
%%-----------------------------------------------------------------------------
parallel_startsolve(Alllines, NumCases) ->
  % start sending cases off to remote solvers
  SentCount = parallel_solvecases(Alllines, 1),
  io:format("~b cases sent to parallel solvers, starting receive loop~n", [SentCount]),
  parallel_receive_loop(NumCases, []).

parallel_solvecases([], Casenum) ->
  %return the sent count for debug output
  Casenum - 1;
parallel_solvecases(Alllines, Casenum) ->
  % split the first few lines off the input for this case
  {CaseLines, RestLines} = lists:split(?LINESPERCASE, Alllines),

  % send these case lines to a newly spawned parallel_solver process
  spawn(?MODULE, parallel_solver, [self(), CaseLines, Casenum]),

  % recurse and spawn the rest
  parallel_solvecases(RestLines, Casenum + 1).

% the parallel solver solves just a single case and sends a message back to the
%   calling process. The case # is attached because they can return out of order.
parallel_solver(From, CaseLines, Casenum) ->
  % single isolated solve
  Result = single_solvecase(CaseLines),
  % report result back to collector
  From ! {result, Casenum, Result}.

% this looping method receives each result when it comes back from a solving process
parallel_receive_loop(NumCases, Results) when NumCases == length(Results) ->
  io:format("Received all results!~n"),
  % sort 'em
  SortedResults = lists:sort(
    fun({CaseA, _ResultA}, {CaseB, _ResultB}) -> CaseA =< CaseB end,
    Results),
  % return just the list of results in casenumber order
  [Result || {_Casenum, Result} <- SortedResults];
parallel_receive_loop(NumCases, Results) ->
  % print status
  io:format("Waiting for ~3b more results.~n", [NumCases - length(Results)]),

  % wait for the next result message to arrive
  receive
    {result, Casenum, Result} ->
      io:format("Received Case #~b~n", [Casenum]),
      print_status(length(Results), NumCases),
      parallel_receive_loop(NumCases, [{Casenum, Result} | Results]);
    Else ->
      io:format("Received bad message format from someone: ~p~n", [Else]),
      {error, bad_message, Else}
  end.

%% util method which parses a line into a list of ints
parse_ints(Line) ->
  Tokens = string:tokens(Line, " "),
  lists:map(fun(Token) -> parse_int(Token) end, Tokens).

%% util method which parses a string with a single value into an int
parse_int(Line) ->
  {Result, _Rest} = string:to_integer(Line),
  Result.

% util method for printing our progress through the cases to solve
print_status(Current, NumCases) ->
  io:format("Solved: ~4b of ~4b cases: ~3b%~n", [Current, NumCases, trunc(Current / NumCases * 100)]),
  ok.

% util method to take a list of return a set of {Index, Item} tuples, where the index is 1-indexed
index_list(List) ->
  lists:zip(lists:seq(1, length(List)), List).
%%-----------------------------------------------------------------------------
%% Problem specific methods after this point, customize for the problem
%%-----------------------------------------------------------------------------

% The actual solver of a single set of case input lines, typically will parse
%   the input lines into a data structure and then work on it
% Customize to the problem
single_solvecase(CaseLines) ->
  % parse the input lines into usable data structures
  {Credit, Items} = parsecase(CaseLines),
  % brute force breadth-first search to solve a case
  {success, {FirstIndex, FirstItem}, {SecondIndex, SecondItem}} = search(Credit, Items),

  % assert the solution is correct if possible
  Credit = FirstItem + SecondItem,

  {FirstIndex, SecondIndex}.


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

%% Case solving algorithm, typically some kind of generate and test or
%%   space searcher
search(_Credit, []) ->
  %should be caught before this, but just in case
  {failure};
search(_Credit, [_Item]) ->
  %only one item left, fail
  {failure};
search(Credit, [{FirstIndex, FirstItem} | Rest]) ->
  Match = Credit - FirstItem,
  % we know what the match would be if it's paired with the first item
  % so search for it
  case lists:keyfind(Match, 2, Rest) of
    {SecondIndex, Match} when SecondIndex > FirstIndex ->
      %we found a match in the list, return success
      {success, {FirstIndex, FirstItem}, {SecondIndex, Match}};
    {SecondIndex, Match} when FirstIndex > SecondIndex ->
      %we found a match in the list, return success
      {success, {SecondIndex, Match}, {FirstIndex, FirstItem}};
    _Else ->
      %look in Rest for a match
      search(Credit, Rest)
  end.

unit_test() ->
  ok.
