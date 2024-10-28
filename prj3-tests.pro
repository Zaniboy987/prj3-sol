#!/usr/bin/env -S prolog

:- module('prj3_tests', []).

:- use_module('prj3-sol.pro').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% assoc/3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-begin_tests(assoc, []).
test(empty, [fail]) :-
    assoc([], a, _).
test(ok, [all(Val=[[x, 2]])]) :-
    Key = x,
    Val = [x, 2],
    Assoc = [[a, 2], Val, [c, 3]],
    assoc(Assoc, Key, Val).
test(fail, [fail]) :-
    Key = y,
    Val = [x|2],
    Assoc = [[a, 2], Val, [c, 3]],
    assoc(Assoc, Key, Val).
test(first, [all(Val=[[x, 2]])]) :-
    Key = x,
    Val = [x, 2],
    Assoc = [Val, [a, 2], [c, 3]],
    assoc(Assoc, Key, Val).
test(last, [all(Val=[[x|2]])]) :-
    Key = x,
    Val = [x|2],
    Assoc = [[a, 2], [c, 3], Val],
    assoc(Assoc, Key, Val).
:-end_tests(assoc).


%%%%%%%%%%%%%%%%%%%%%%%%%%% divisible_element/3 %%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(divisible_element, []).
test(three, [all(X == [6, 3, 27])]) :-
    divisible_element([2, 1, 6, 5, 3, 8, 27, 2], 3, X).
test(five, [all(X == [5, 5, 25])]) :-
    divisible_element([5, 1, 6, 5, 3, 8, 25, 2], 5, X).
test(fail, [fail]) :-
    divisible_element([5, 1, 6, 5, 3, 8, 25, 2], 7, _X).
:- end_tests(divisible_element).

%%%%%%%%%%%%%%%%%%%%%%%%%%% select_divisible/3 %%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(select_divisible, []).
test(three, [all(X == [[6, 3, 27]])]) :- 
    select_divisible([2, 1, 6, 5, 3, 8, 27, 2], 3, X).
test(five, [all(X == [[5, 5, 25]])]) :-
    select_divisible([5, 1, 6, 5, 3, 8, 25, 2], 5, X).    
test(none, [all(X == [[]])]) :-
    select_divisible([5, 1, 6, 5, 3, 8, 25, 2], 7, X).
:- end_tests(select_divisible).

%%%%%%%%%%%%%%%%%%%%%%%%%%% has_length_n_lists %%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(has_length_n_lists, []).
test(len0_2, [nondet]) :-
    has_length_n_lists([], 2).
test(len1_2, [nondet]) :-
    has_length_n_lists([[a, b]], 2).
test(len2_2, [nondet]) :-
    has_length_n_lists([[a, b], [a, b, c, d]], 2).
test(len3_1, [nondet]) :-
    has_length_n_lists([[a, b], [a, b, c], [a, b, c, d]], 1).
test(len4_m1, [nondet]) :-
    has_length_n_lists([[a, b, c, d], [a, b, c], [a, b], [a]], -1).
test(len4_m1_fail, [fail]) :-
    has_length_n_lists([[a, b, c, d], [a, b, c], [a, b], [b, a]], -1).
:- end_tests(has_length_n_lists).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% poly_eval/3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(poly_eval, []).
test(empty2, [true(V =:= 0)]) :-
    poly_eval([], 2, V).
test(const2, [true(V =:= 5)]) :-
    poly_eval([5], 2, V).
test(linear2, [true(V =:= 19)]) :-
    poly_eval([5, 7], 2, V).
test(quadratic2, [true(V =:= 35)]) :-
    poly_eval([5, 7, 4], 2, V).
test(quartic2, [true(V =:= 67)]) :-
    poly_eval([5, 7, 4, 2, 1], 2, V).
test(quartic2_neg, [true(V =:= 7)]) :-
    poly_eval([5, 7, 4, 2, 1], -2, V).
test(quartic5, [true(V =:= 5 + 7*5 + 4*5^2 + 2*5^3 + 1*5^4)]) :-
    poly_eval([5, 7, 4, 2, 1], 5, V).
:- end_tests(poly_eval).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% path/4 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dag1([edge(1, 2), edge(1, 3), edge(2, 4), edge(3, 4)]).
dag2(Dag2) :-
    dag1(Dag1), append([edge(0, 1), edge(4, 5)], Dag1, Dag2).

:- begin_tests(path, []).
test(dag1_path0, [set(Path = [[]])]) :-
    dag1(Dag1), 
    path(Dag1, 2, 2, Path).
test(dag1_path1, [set(Path = [[edge(2, 4)]])]) :-
    dag1(Dag1), 
    path(Dag1, 2, 4, Path).
test(dag1_path2, [set(Path = [[edge(1, 2), edge(2, 4)],
			 [edge(1, 3), edge(3, 4)]
			])]) :-
    dag1(Dag1), 
    path(Dag1, 1, 4, Path).
test(dag2_path2, [set(Path = [[edge(1, 2), edge(2, 4)],
			 [edge(1, 3), edge(3, 4)]
			])]) :-
    dag2(Dag2), 
    path(Dag2, 1, 4, Path).
test(dag2_path3, [set(Path = [[ edge(0, 1), edge(1, 2), edge(2, 4)],
			 [ edge(0, 1), edge(1, 3), edge(3, 4)]
			])]) :-
    dag2(Dag2), 
    path(Dag2, 0, 4, Path).
test(dag2_path4,
     [set(Path = [[ edge(0, 1), edge(1, 2), edge(2, 4), edge(4, 5)],
		  [ edge(0, 1), edge(1, 3), edge(3, 4), edge(4, 5)]
		 ])]) :-
    dag2(Dag2), 
    path(Dag2, 0, 5, Path).
test(dag1_fail0, [fail]) :-
    dag1(Dag1), 
    path(Dag1, 2, 3, _Path).
test(dag2_fail1, [fail]) :-
    dag2(Dag2), 
    path(Dag2, 3, 2, _Path).
:- end_tests(path).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% re_match/2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(re_match, []).

% "1" =~ /1/
test(atomic, [nondet]) :-
    re_match(1, [1]).

% "1" =~ /1/
test(seq1, [nondet]) :-
    re_match([1], [1]).

% "11" =~ /11/
test(seq2, [nondet]) :-
    re_match([1, 1], [1, 1]).

% "11" !~ /12/
test(seq2_fail, [fail]) :-
    re_match([1, 1], [1, 2]).

% "1a2" =~ /1[abc]2/
test(seq_cclass_0, [nondet]) :-
    re_match([1, cclass([a, b, c]), 2], [1, a, 2]).

%  "1b2" =~ /1[abc]2/
test(seq_cclass_1, [nondet]) :-
    re_match([1, cclass([a, b, c]), 2], [1, b, 2]).

%  "1c2" =~ /1[abc]2/
test(seq_cclass_2, [nondet]) :-
    re_match([1, cclass([a, b, c]), 2], [1, c, 2]).

%  "112" !~ /1[abc]2/
test(seq_cclass_2_fail, [fail]) :-
    re_match([1, cclass([a, b, c]), 2], [1, 1, 2]).

% "a" =~ /[ab]|[01]/
test(alt_cclass_0, [nondet]) :-
    re_match(alt(cclass([a, b]), cclass([0, 1])), [a]).

% "b" =~ /[ab]|[01]/
test(alt_cclass_1, [nondet]) :-
    re_match(alt(cclass([a, b]), cclass([0, 1])), [b]).

% "0" =~ /[ab]|[01]/
test(alt_cclass_2, [nondet]) :-
    re_match(alt(cclass([a, b]), cclass([0, 1])), [0]).

% "1" =~ /[ab]|[01]/
test(alt_cclass_3, [nondet]) :-
    re_match(alt(cclass([a, b]), cclass([0, 1])), [1]).

% "abc" =~ /(ab*c|ab)c/
test(seq_alt_back1, [nondet]) :-
    re_match([alt([a, rep(b), c], [a, b]), c], [a, b, c]).

% "abbbc" =~ /(ab*c*|ab)c/
test(seq_alt_back2, [nondet]) :-
    re_match([alt([a, rep(b), rep(c)], [a, b]), c], [a, b, b, b, c]).

% "abbbc" !~ /(ab*c|ab)c/
test(seq_alt_back_fail, [fail]) :-
    re_match([alt([a, rep(b), c], [a, b]), c], [a, b, b, b, c]).

:- end_tests(re_match).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% main/0 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main :-
    current_prolog_flag(argv, Argv),
    %set_test_options([format(log)]),
    (length(Argv, 0) -> run_tests ; run_tests(Argv)).

:-initialization(main, main).