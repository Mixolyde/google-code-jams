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

-module(codejamwelcome2).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

%% Defines to modify per problem

%% Number of lines to split off the input lines per case
-define(LINESPERCASE, 1).

%% Number of result values to output on a case line
-define(RESULTSPERLINE, 1).

%% Quick start method calls
codejamsmalla() -> codejam("A-small-practice.in").
codejamlargea() -> codejam("A-large-practice.in").
codejamsmallb() -> codejam("B-small-practice.in").
codejamlargeb() -> codejam("B-large-practice.in").
codejamsmallc() -> codejam("C-small-practice.in").
codejamlargec() -> codejam("C-large-practice.in").
codejamsample() -> codejam("sample.in").
codejamtemplate() -> codejam("template.in").

% specific macros for this problem
-define(PHRASE, "welcome to code jam").

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
  % Results = sequential_startsolve(Caselines, NumCases),
  Results = parallel_startsolve(Caselines, NumCases),

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
  Format = "Case #~w: ~4..0w~n",
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

%%-----------------------------------------------------------------------------
%% Problem specific methods after this point, customize for the problem
%%-----------------------------------------------------------------------------

% The actual solver of a single set of case input lines, typically will parse
%   the input lines into a data structure and then work on it
% Customize to the problem
single_solvecase([FirstLine]) ->

  Combinations = search(FirstLine),


  {Combinations}.

%% Case solving algorithm, typically some kind of generate and test or
%%   space searcher
search(Line) ->
  search(?PHRASE, index_list(Line), []).

search([], _Line, _Accum = [LastPrefix | _RestPrefixes]) ->
  %return final sum of accumulator prefixes
  % io:format("Final Prefix: ~p~n", [LastPrefix]),
  lists:sum([Count || {_Pos, Count} <- LastPrefix]) rem 10000;
search([Char | RestPhrase], IndexedLine, Accum) ->
  % search first char
  NewAccum = accumulate_prefix_list(Char, IndexedLine, [[] | Accum]),
  % search rest
  search(RestPhrase, IndexedLine, NewAccum).

count_prefixes([], _Position) ->
  1;
count_prefixes([FirstPrefixList | _Prefixes], Position) ->
  lists:sum([Count || {Pos, Count} <- FirstPrefixList, Pos < Position]).

accumulate_prefix_list(_Char, [], AllPrefixes = [_FirstPrefixList | _Prefixes]) ->
  % io:format("Accumlated final list for ~c: ~p~n", [Char, FirstPrefixList]),
  AllPrefixes;
accumulate_prefix_list(Char, [{LinePos, Char} | RestLine], [CurrentPrefixList | Prefixes]) ->
  % since this char matches the char in the line, we build a prefix list add it to the current list
  Count = count_prefixes(Prefixes, LinePos),
  accumulate_prefix_list(Char, RestLine, [[{LinePos, Count} | CurrentPrefixList] | Prefixes]);
accumulate_prefix_list(Char, [{_LinePos, _NotChar} | RestLine], Prefixes) ->
  accumulate_prefix_list(Char, RestLine, Prefixes).

index_list(List) ->
  lists:zip(lists:seq(1, length(List)), List).

unit_test() ->
  [{1, thing}] = index_list([thing]),
  1 = count_prefixes([], 1),
  0 = count_prefixes([[{1, 3}]], 1),
  3 = count_prefixes([[{1, 3}]], 2),
  5 = count_prefixes([[{1, 3}, {2, 2}]], 3),
  5 = count_prefixes([[{1, 3}, {2, 2}, {3, 1}]], 3),
  243 = search("welllcooomeee ttto coddde jam"),

  35067810624133695488 = search("so dqmweawewwwwwewwweoeeecweeeeeeljeeem llleclljllcclccllcocdcccoocoeomc moommmojmm oom ommee e eeeeeceem     ee cj ttwetoe t  oo t  ttoowotootto oo  e oo do   ocl voc c ce cdooococodcmocoeodo ododddoodededddddedtecee de eeem j ee     jr jt jm jjcjjjjjjajoaaaaaaaademmaajmtmmmmmmmdm ommh ei"),
  % 0 = search("so dqmweawewwwwwewwweoeeecweeeeeeljeeem llleclljllcclccllcocdcccoocoeomc moommmojmm oom ommee e eeeeeceem     ee cj ttwetoe t  oo t  ttoowotootto oo  e oo do   ocl voc c ce cdooococodcmocoeodo ododddoodededddddedtecee de eeem j ee     jr jt jm jjcjjjjjjajoaaaaaaaademmaajmtmmmmmmmdm ommh ei"),
  ok.