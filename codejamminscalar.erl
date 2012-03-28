%%%-------------------------------------------------------------------
%%% File    : codejamminscalar.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Find the minimum scalar product possible from two
%%%   arrays of the same length
%%%
%%% Created :  15 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

%% erlang shell commands to run:
%% cd ("C:/Users/bwilliams/Dropbox/dev/erl/codejam").
%% c(codejamminscalar).
%% codejamminscalar:codejam("A-small-practice.in").

-module(codejamminscalar).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

%% Number of lines to split off the input lines per case
-define(LINESPERCASE, 3).

%% Number of result values to output on a case line
-define(RESULTSPERLINE, 1).

%%-----------------------------------------------------------------------------
%% Function: codejame/1
%% Purpose: Solve the codejam problem in the provided input file
%% Args: InFilename is the path/name of the input file
%% Returns: ok or {error, Reason} if it crashes
%%-----------------------------------------------------------------------------
codejam(InFilename) ->
  io:format("Opening ~s for input.~n", [InFilename]),
  % substring out the "main" part of the filename (part before the .in)
  MainFilename = string:substr(InFilename, 1, string:str(InFilename, ".in") - 1),
  % create an outputfile name like main_name.out
  OutFilename = MainFilename ++ ".out",

  {ok, InD} = file:open(InFilename, [read]),

  % read all the lines of the file into a list of lines in memory
  % split the first line off, as it is almost always the number of cases in the file
  [NumCaseString | Caselines] = get_all_lines(InD, []),
  {NumCases, []} = string:to_integer(NumCaseString),


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
  io:format("Opening ~s for output.~n", [OutFilename]),
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
%% Args: List of input lines to solve
%% Returns: List of results for each case or {error, Reason} if it crashes
%%-----------------------------------------------------------------------------
solvecases(Caselines) ->
  % start the recursive solver at Case #1 and empty result accumulator
  solvecase(Caselines, []).

solvecase([], Accum) -> lists:reverse(Accum); %termination case, reverse the results and return
solvecase(Lines, Accum) ->
  % io:format("DEBUG: Solving Case #~w~n", [NumCases]),

  % split the first few lines off the input for this case
  {CaseLines, RestLines} = lists:split(?LINESPERCASE, Lines),

  % parse the input lines into usable data structures
  {First, Second} = parsecase(CaseLines),

  % now that we've parsed, filtered and sorted the data file, we can actually solve the problem!

  % zip the multiplications up
  Zipped = lists:zipwith(fun(X, Y) -> X*Y end, First, Second),

  % sum them
  MinScalar = lists:foldl(fun(X, Sum) -> X + Sum end, 0, Zipped),

  solvecase(RestLines, [{MinScalar} | Accum]).

%%-----------------------------------------------------------------------------
%% Function: parsecase/1
%% Purpose: Takes a list of case lines and parses them into the data variables
%%   needed to solve the problem
%% Args: List of input lines to parse
%% Returns: Tuple of values for solving or an error
%%-----------------------------------------------------------------------------
parsecase([SizeLine, FirstArrayLine, SecondArrayLine]) ->
  {Size, []} = string:to_integer(SizeLine),
  First = parse_ints(FirstArrayLine),
  Second = parse_ints(SecondArrayLine),
  Size = length(First),
  Size = length(Second),
  SortedFirst = lists:sort(fun(A, B) -> A =< B end, First),
  % sort in reverse order
  SortedSecond = lists:sort(fun(A, B) -> B =< A end, Second),
  {SortedFirst, SortedSecond}.

%% parses a line into ints
parse_ints(Line) ->
  Tokens = string:tokens(Line, " "),
  lists:map(fun(Token) -> {Integer, []} = string:to_integer(Token), Integer end, Tokens).


%% Case solving algorithm
bfs(_Credit, [_Item]) ->
  %only one item left, fail
  {failure};
bfs(Credit, [{FirstIndex, FirstItem} | Rest]) ->
  Match = Credit - FirstItem,
  % we know what the match would be if it's paired with the first item
  % so search for it
  case lists:keyfind(Match, 2, Rest) of
    {SecondIndex, Match} ->
      %we found a match in the list, return success
      {success, {FirstIndex, FirstItem}, {SecondIndex, Match}};
    _Else ->
      %look in Rest for a match
      bfs(Credit, Rest)
  end.
