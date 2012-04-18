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

-module(codejamtongues).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

%% Number of lines to split off the input lines per case
-define(LINESPERCASE, 1).

%% Number of result values to output on a case line
-define(RESULTSPERLINE, 1).

-define(CIPHER, [
{$a, $y}, {$b, $n}, {$d, $i}, {$c, $f}, {$e, $c},
{$f, $w}, {$g, $l}, {$h, $b}, {$i, $k}, {$j, $u},
{$k, $o}, {$l, $m}, {$m, $x}, {$n, $s}, {$o, $e},
{$p, $v}, {$q, $z}, {$r, $p}, {$s, $d}, {$t, $r},
{$u, $j}, {$v, $g}, {$w, $t}, {$x, $h}, {$y, $a},
{$z, $q},
{$ , $ }]).

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
  Format = "Case #~w:" ++ string:copies(" ~s", ?RESULTSPERLINE) ++ "~n",
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
solvecase(Lines, Accum) ->
  % io:format("DEBUG: Solving Case #~w~n", [NumCases]),

  % split the first few lines off the input for this case
  {[CaseLine], RestLines} = lists:split(?LINESPERCASE, Lines),

  CipherText = cipher(CaseLine),

  % push the result onto the accumulator and recurse
  solvecase(RestLines, [{CipherText} | Accum]).

cipher(PlainText) ->
  cipher(PlainText, []).
cipher([], Accum) -> lists:reverse(Accum);
cipher([CipherLetter | Rest], Accum) ->
  case lists:keyfind(CipherLetter, 2, ?CIPHER) of
    {Letter, CipherLetter} ->
      cipher(Rest, [Letter | Accum]);
    false ->
      cipher(Rest, [$# | Accum])
  end.

%% parses a line into ints
parse_ints(Line) ->
  Tokens = string:tokens(Line, " "),
  lists:map(fun(Token) -> parse_int(Token) end, Tokens).

%% parse a line with single value into an int
parse_int(Line) ->
  {Result, _Rest} = string:to_integer(Line),
  Result.

unit_test() ->
  ok.

