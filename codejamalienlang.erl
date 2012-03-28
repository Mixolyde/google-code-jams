%%%-------------------------------------------------------------------
%%% File    : codejamalienlang.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Google Code Jam solution for alien language problem
%%%   from Qual round 2009
%%%
%%% Created :  21 Mar 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

%% erlang shell commands to run:
%% cd ("C:/Users/bwilliams/Dropbox/dev/erl/codejam").
%% c(codejamalienlang).

-module(codejamalienlang).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

%% Number of lines to split off the input lines per case
-define(LINESPERCASE, 1).

%% Number of result values to output on a case line
-define(RESULTSPERLINE, 1).

% Quick start method calls
codejamsmall()  -> codejam("A-small-practice.in").
codejamlarge()  -> codejam("A-large-practice.in").
codejamsample() -> codejam("sample.in").

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
  [NumCaseString | Restlines] = get_all_lines(InD, []),
  [_WordLength, NumKnownWords, NumCases] = parse_ints(NumCaseString),

  % parse all known words
  {KnownWords, CaseLines} = lists:split(NumKnownWords, Restlines),
  io:format("Known words: ~p~n", [KnownWords]),

  % send all of the data lines to the solver
  Results = solvecases(KnownWords, CaseLines),

  % assert that we have the right number of results
  NumCases = length(Results),
  % debug result print
  io:format("DEBUG: Results: ~p~n", [Results]),

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
solvecases(KnownWords, Caselines) ->
  % start the recursive solver at Case #1 with empty result accumulator
  solvecase(KnownWords, Caselines, []).

solvecase(_KnownWords, [], Accum) -> lists:reverse(Accum); %termination case, reverse the results and return
solvecase(KnownWords, [CaseLine | RestLines], Accum) ->
  % io:format("DEBUG: Solving Case #~w~n", [NumCases]),

  % parse the input line into usable data structures
  % example: ["ab", "c", "de"]
  Listoflists = parsecase(CaseLine),
  
  % now that we've parsed, filtered and sorted the data file, we can actually solve the problem!

  %tried and true AI technique of generate and test
  %use set of sets module to generate cartesian product of all results
  % Listofsofssets = [sofs:set(Set) || Set <- Listoflists],
  % Cartesian = lists:map(fun(External) -> tuple_to_list(External) end, 
  %   sofs:to_external(sofs:product(list_to_tuple(Listofsofssets)))),
  % io:format("Cartesian: ~p~n", [Cartesian]),
  % filter out results that are not known words
  % FilteredCartesian = lists:filter(fun(Elem) -> lists:member(Elem, KnownWords) end, Cartesian),
  % io:format("FilteredCartesian: ~p~n", [FilteredCartesian]),

  % generate and test method took way too much space and memory with 25^9 possible combinations to test
  % so we'll do some depth first searching with short circuits
  Solutions = dfs(KnownWords, Listoflists),
  % io:format("Solution list: ~w~n", [Solutions]),

  solvecase(KnownWords, RestLines, [{length(Solutions)} | Accum]).

dfs(KnownWords, []) -> KnownWords; % termination case, out of letters to match, return the survivors
dfs(KnownWords, [FirstList | Restoflists ]) ->
  % we take the list of known words, filter out the ones that start with each of the possible letters
  % in FirstList, and for each one, recurse on the remaining portion of that word for possible solutions
  SubSearches = dfs_filter(KnownWords, FirstList),
  % io:format("SubSearches: ~w~n", [SubSearches]),
  % now search the filtered known words
  % so search (["ace"], ["a", "c", "e"]) becomes search (["ce"], ["c", "e"])
  dfs(SubSearches, Restoflists).

% return a list of known words that would be matched by one of the letters in the list
% if we run out of known words or letters to search with, return results
dfs_filter(KnownList, Chars) ->
  [RestKnown || [FirstKnown | RestKnown] <- KnownList, Char <- Chars, FirstKnown == Char].
  
%%-----------------------------------------------------------------------------
%% Function: parsecase/1
%% Purpose: Takes a list of case lines and parses them into the data variables
%%   needed to solve the problem
%% Args: List of input lines to parse
%% Returns: Tuple of values for solving or an error
%%-----------------------------------------------------------------------------
parsecase(Chars) -> parsecase(Chars, [], false).

% (ab)c(de) should return [[a, b], [c], [d, e]]
% (ab)(cd) should return [[a, b], [c, d]]
parsecase([], Results, _InParens) -> lists:reverse(Results); %termination clause, reverse parse results and return
% new group, push an empty list to populate
parsecase([ $( | Rest], Results, _InParens) ->
  parsecase(Rest, [[]] ++ Results, true);
% end of group, no longer in parens
parsecase([ $) | Rest], Results, _InParens) ->
  parsecase(Rest, Results, false);
%when we get a single character and we're currently in a paren group
parsecase([Other | Rest], [Current | RestResults], true) ->
  parsecase(Rest, [Current ++ [Other] | RestResults], true);
%when we get a single char and we're not in a paren group
parsecase([Other | Rest], RestResults, false) ->
  parsecase(Rest, [[Other]] ++ RestResults, false).


%% parses a line into ints
parse_ints(Line) ->
  Tokens = string:tokens(Line, " "),
  lists:map(fun(Token) -> parse_int(Token) end, Tokens).

%% parse a string with a single value into an int
parse_int(Line) ->
  {Result, _Rest} = string:to_integer(Line),
  Result.


unit_test() ->
  ok = unit_test_parsers(),
  [] = dfs_filter(["ace", "bcd"], ""),
  ["ce"] = dfs_filter(["ace", "bcd"], "a"),
  ["ce", "cd"] = dfs_filter(["ace", "bcd"], "ab"),
  ["ce", "cd"] = dfs_filter(["ace", "bcd"], "abcdefg"),
  [[], []] = dfs(["ce", "cd"], ["c", "ed"]),
  [{0}] = solvecase([], ["(ab)c(de)"], []),
  [{1}] = solvecase(["ace"], ["(ab)c(de)"], []),
  [{2}] = solvecase(["ace", "bcd"], ["(ab)c(de)"], []),
  [{0}] = solvecase(["nwlrbbmqbh"], ["(opqrsvwxyzabcdefghijklmn)(yzabcdefgijklmnpqrstuvwx)(ijklmopqrstuvwxzabcefgh)(stuvwxyzabcdefhijklmnopq)(mnopqrstuvwxyzabdefghijkl)(ijklmnopqrstuvwxzbcdefh)(stuvwzabcdefgijklmnopq)(efghijklmnopqrsuvwxyacd)(rstuvxyzadefghijklmnopq)(noqrsuwxyzabdfghjlm)"], []),
  ok.

unit_test_parsers() ->
  123 = parse_int("123 "),
  [1] = parse_ints("1 "),
  [1] = parse_ints("1"),
  [1] = parse_ints(" 1 "),
  [1, 2] = parse_ints(" 1 2 "),
  ["ab", "c", "de"] = parsecase("(ab)c(de)"),
  ["a", "b", "c", "d", "e"] = parsecase("abcde"),
  ["a", "bc", "d", "e"] = parsecase("a(bc)de"),
  ["ab", "de"] = parsecase("(ab)(de)"),
  ["abde"] = parsecase("(abde)"),
  ok.

