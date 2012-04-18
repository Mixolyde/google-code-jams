%%%-------------------------------------------------------------------
%%% File    : codejamtemplate.erl
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

-module(codejamrecycle).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

%% Number of lines to split off the input lines per case
-define(LINESPERCASE, 1).

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
  Results = solvecases(Caselines, NumCases),

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
  Format = "Case #~w:" ++ string:copies(" ~w", ?RESULTSPERLINE) ++ "~n",
  io:format(Format, OutputList),
  io:format(OutD, Format, OutputList),
  print_output_device(OutD, Rest, Count + 1).


%%-----------------------------------------------------------------------------
%% Function: solvecases/1
%% Purpose: Splits the input lines into input groups and solves the problem
%% Args: List of input lines to solve, total number of cases for reporting status
%% Returns: List of results for each case or {error, Reason} if it crashes
%%-----------------------------------------------------------------------------
solvecases(Caselines, NumCases) ->
  % start the recursive solver at Case #1 with empty result accumulator
  solvecase(Caselines, NumCases, []).

solvecase([], NumCases, Accum) ->
  print_status(length(Accum), NumCases),
  lists:reverse(Accum); %termination case, reverse the results and return
solvecase(Lines, NumCases, Accum) ->
  print_status(length(Accum), NumCases),
  % io:format("DEBUG: Solving Case #~w~n", [NumCases]),

  % split the first few lines off the input for this case
  {CaseLines, RestLines} = lists:split(?LINESPERCASE, Lines),

  % parse the input lines into usable data structures
  {A, B} = parsecase(CaseLines),
  % io:format("DEBUG: Have ~w to spend on ~w items: ~w~n", [Credit, length(Items), Items]),

  % now that we've parsed, filtered and sorted the data file, we can actually solve the problem!

  % brute force breadth-first search to solve a case
  Count = search(A, B),

  % push the result onto the accumulator and recurse
  solvecase(RestLines, NumCases, [{Count} | Accum]).

%%-----------------------------------------------------------------------------
%% Function: parsecase/1
%% Purpose: Takes a list of case lines and parses them into the data variables
%%   needed to solve the problem
%% Args: List of input lines to parse
%% Returns: Tuple of values for solving or an error
%%-----------------------------------------------------------------------------
parsecase([CaseLine]) ->
  [A, B] = parse_ints(CaseLine),
  {A, B}.

%% parses a line into ints
parse_ints(Line) ->
  Tokens = string:tokens(Line, " "),
  lists:map(fun(Token) -> parse_int(Token) end, Tokens).

%% parse a line with single value into an int
parse_int(Line) ->
  {Result, _Rest} = string:to_integer(Line),
  Result.

print_status(Current, NumCases) ->
  io:format("Solved: ~4b of ~4b cases: ~3b%~n", [Current, NumCases, trunc(Current / NumCases * 100)]),
  ok.

%% Case solving algorithm
search (A, A) -> 0;
search (A, B) ->
  search(A, B, A, []).

% bubble search all the pairs between A and B
search(_A, B, Index, Accum) when Index == B - 1 ->
  % io:format("Accums: ~p~n", [Accum]),
  length(Accum);
search(A, B, Index, Accum) ->
  search(A, B, Index + 1, Accum ++ recycle_pairs(A, B, Index)).

recycle_pairs(A, B, Int) ->
  %convert Ints to strings
  IntString = lists:flatten(io_lib:format("~b", [Int])),
  % AString = lists:flatten(io_lib:format("~b", [A])),
  % BString = lists:flatten(io_lib:format("~b", [B])),
  % io:format("Int: ~b, IntString ~s~n", [Int, IntString]),
  % generate all the shifts for this int and test them for validity
  ShiftedSplits = [ lists:split(Shift, IntString) || Shift <- lists:seq(1, length(IntString) - 1)],
  % filter out the ones that would start with 0
  FilteredSplits = lists:filter(
    fun({_First, [SecondLetter | _RestSecond]} ) ->
      SecondLetter /= $0 end, ShiftedSplits),
  % perform the shift and convert back to integers
  ShiftedInts = lists:map(
    fun({First, Second}) -> parse_int(Second ++ First) end,
    FilteredSplits),
  % test each generated int for validity
  ValidPairs = lists:filter(
    % Test that A <= Int < TestInt =< B
    fun(TestInt) -> A =< TestInt andalso Int < TestInt andalso TestInt =< B
    end, ShiftedInts),
  % print some debug info
  case length(ValidPairs) > 0 of
    true ->
      % io:format("Int: ~b Shifts: ~p Valids: ~p~n", [Int, ShiftedInts, ValidPairs]),
      ok;
    false -> ok
  end,
  % finally, return the number of valid pairs
  [{Int, ValidShift} || ValidShift <- ValidPairs].

unit_test() ->
  0 = length(recycle_pairs(11, 13, 12)),
  0 = length(recycle_pairs(11, 13, 1)),
  1 = length(recycle_pairs(11, 25, 12)),
  1 = length(recycle_pairs(11, 300, 123)),
  2 = length(recycle_pairs(11, 400, 123)),
  0 = search(1, 9),
  3 = search(10, 40),
  156 = search(100, 500),
  287 = search(1111, 2222),
  ok.
