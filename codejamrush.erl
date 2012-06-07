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

-module(codejamrush).
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
  [Count, Result] = OutputList,
  case Result > 0 of
    true ->
      Format = "Case #~w:" ++ string:copies(" ~w", ?RESULTSPERLINE) ++ "~n",
      io:format(Format, OutputList),
      io:format(OutD, Format, OutputList);
    false ->
      Format = "Case #~w: Too Bad~n",
      io:format(Format, [Count]),
      io:format(OutD, Format, [Count])

  end,
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
sequential_solvecase([LineCountLine | Lines], NumCases, Accum) ->
  print_status(length(Accum), NumCases),     % print the current status

  LineCount = parse_int(LineCountLine),
  % split the next problem input lines off the input for this case
  {CaseLines, RestLines} = lists:split(LineCount, Lines),

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
parallel_solvecases([LineCountLine | Lines], Casenum) ->
  LineCount = parse_int(LineCountLine),
    % split the next problem input lines off the input for this case
  {CaseLines, RestLines} = lists:split(LineCount, Lines),

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
  LevelData = parsecase(CaseLines),
  % brute force breadth-first search to solve a case
  Result = search(LevelData),

  {Result}.


%%-----------------------------------------------------------------------------
%% Function: parsecase/1
%% Purpose: Takes a list of case lines and parses them into the data variables
%%   needed to solve the problem
%% Args: List of input lines to parse
%% Returns: Tuple of values for solving or an error
%%-----------------------------------------------------------------------------
parsecase(Lines) ->
  parsecase(Lines, 1, []).

parsecase([Line | Rest], Count, Accum) ->
  [OneStar, TwoStar] = parse_ints(Line),
  parsecase(Rest, Count + 1, [{Count, OneStar, TwoStar} | Accum]);
parsecase([], _Count, Accum) ->
  lists:reverse(Accum).

%% Case solving algorithm, typically some kind of generate and test or
%%   space searcher
search(LevelData) ->
  search(lists:keysort(3, LevelData), 0, 0, []).

search(LevelData, Stars, GamesPlayed, Completed) when length(Completed) == (2 * length(LevelData)) ->
  io:format("Completed ~w with Games: ~w and Stars: ~w~n", [Completed, GamesPlayed, Stars]),
  GamesPlayed;
search(LevelData, Stars, GamesPlayed, Completed) ->
  % io:format("Searching Completed ~p with Games: ~p and Stars: ~p~n", [Completed, GamesPlayed, Stars]),
  % get the list of possible level completetions
  Poss_Moves = possible_moves(LevelData, Stars, []),
  Moves = filter_moves(Poss_Moves, Completed),
  % io:format("Moves: ~p~n", [Moves]),
  case length(Moves) == 0 of
    true -> 0;
    false ->
      %pick best Move and recurse
      case choose_move(Moves, Completed) of
        {{LevelNum, 2}, _NotUsed} ->
          %found a 2 star we can complete
          {NewStars, NewCompleted} = complete_two({LevelNum, 2}, Completed),
          search(LevelData, Stars + NewStars, GamesPlayed + 1, NewCompleted);
        {LevelNum, 1} ->
          search(LevelData, Stars + 1, GamesPlayed + 1, [{LevelNum, 1} | Completed])
      end
  end.

choose_move(Moves, Completed) ->
  {TwoStars, OneStars} = lists:partition(fun({_Level, Star}) -> Star == 2 end, Moves),
  case length(TwoStars) > 0 of
    true ->
      UnCompletedTwos = lists:filter(fun({Level, 2}) -> lists:member({Level, 1}, Completed) == false end, TwoStars),
      case length(UnCompletedTwos) > 0 of
        true -> {lists:nth(1, UnCompletedTwos), uncompleted};
        false ->{lists:nth(1, TwoStars), completed}
      end;
    false ->
      lists:nth(1, OneStars)
  end.

% figure out the possible level completions at this state as a list of {Level, # of stars that can complete} pairs
possible_moves([{Count, _OneStar, TwoStar} | Rest], Stars, Accum) when Stars >= TwoStar ->
  possible_moves(Rest, Stars, [{Count, 2} | Accum]);
possible_moves([{Count, OneStar, _TwoStar} | Rest], Stars, Accum) when Stars >= OneStar ->
  possible_moves(Rest, Stars, [{Count, 1} | Accum]);
possible_moves([_Level | Rest] , Stars, Accum) ->
  possible_moves(Rest, Stars, Accum);
possible_moves([], _Stars, Accum) ->
  Accum.

filter_moves(Moves, [{Level, 2} | Rest]) ->
  DeletedTwo = lists:delete({Level, 2}, Moves),
  filter_moves(DeletedTwo, Rest);
filter_moves(Moves, [{Level, 1} | Rest]) ->
  Deleted = lists:delete({Level, 1}, Moves),
  filter_moves(Deleted, Rest);
filter_moves(Moves, []) -> Moves.

complete_two({LevelNum, 2}, Completed) ->
  case lists:keyfind(LevelNum, 1, Completed) of
    {LevelNum, 1} ->
      %already completed one, update it to two, return one new star
      {1, [{LevelNum, 2} | Completed]};
    false ->
      %no matching level 2, add both
      {2, [{LevelNum, 2}, {LevelNum, 1} | Completed]}
  end.

unit_test() ->
  ok.

