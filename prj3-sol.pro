#!/usr/bin/env -S swipl
%-*- mode: prolog; -*-

:- module(prj3_sol,  [
     assoc/3,
     divisible_element/3,
     select_divisible/3,
     has_length_n_lists/2,
     poly_eval/3,
     path/4,
     re_match/2
  ]).



% #1: "10-points" 
% assoc(List, Head, Element): Element is a "list" element of List
% having a head matching Head (similar to Scheme assoc()).
%assoc(_List, _Head, _Element) :- 'TODO'.

assoc([], _, _) :- fail.                   % When list is empty

assoc([[Head|Tail]|_], Head, [Head|Tail]). % If 'head' of current list = Head, return

assoc([_|Rem], Head, Element) :-           % If 'head' != Head, continue searching
    assoc(Rem, Head, Element).



% #2: "10-points"
% divisible_element(IntList, N, DivElement): DivElement is an
% element of integer list IntList which is divisible by N.
% Solutions must be produced in the same order in which they occur
% in IntList.
% Hint: A mod B results in the modulus of A wrt B when used
% in an arithmetic context.
% *Restriction*: must be a single rule which cannot be recursive.
%divisible_element(_IntList, _N, _DivElement) :- 'TODO'.

divisible_element(IntList, N, DivElement) :-
    member(DivElement, IntList),             % Find elem in list
    0 =:= DivElement mod N.



% #3: "10-points"
% select_divisible(IntList, N, DivList): DivList is a sub-list of
% those elements of integer list IntList which are divisible by
% integer N.
% The elements in DivList must be in the same order in which they occur
% in IntList.
%select_divisible(_IntList, _N, _DivList) :- 'TODO'.

select_divisible([], _, []).                        % Empty list = empty divisible list

select_divisible([Head|Tail], N, [Head|DivTail]) :- % If head of IntList divisible by N, include it in DivList
    0 =:= Head mod N,
    select_divisible(Tail, N, DivTail).

select_divisible([Head|Tail], N, DivList) :-        % Skip if head of IntList not divisible by N
    Head mod N =\= 0,
    select_divisible(Tail, N, DivList).



% #4: "15-points"
% has_length_n_lists(Lists, N): Lists is a list of lists
% such that the length of the next list is N + the length
% of the previous list.
%has_length_n_lists(_Lists, _N) :- 'TODO'.

has_length_n_lists([], _).                 % empty list
has_length_n_lists([_], _).                % single list (any len)

has_length_n_lists([One, Two | Rem], N) :- % If diff between len of 1st & 2nd list = N, continue checking
    my_length(One, L1),
    my_length(Two, L2),
    L2 =:= L1 + N,
    has_length_n_lists([Two | Rem], N).

my_length([], 0).
my_length([_|Xs], L) :-                    % Helper to calc len of list
    my_length(Xs, LXs),
    succ(LXs, L).



% #5: "15-points"
% poly_eval(Coeffs, X, Eval): Given a numeric list Coeffs of
% polynomial coefficients [ C_0, C_1, C_2, ... ], Eval
% matches the value of the polynomial at X, namely
% C_0*X^0 + C_1*X^1 + C_2*X^2 + C_3*X^3.
% *Restriction*: must be tail-recursive and cannot use exponentiation.
%poly_eval(_Coeffs, _X, _Eval) :- 'TODO'.
  
poly_eval(Coeffs, X, Eval) :-
    starter(Coeffs, X, Initialize),
    polynomial(Coeffs, X, Initialize, Eval).

starter(_, _, state(0, 1)).                                            % Initialize state to Acc = 0, XPower = 1

polynomial([], _, state(Acc, _), Acc).                                 % Ret Acc value

polynomial([C|Cs], X, state(Acc, XPower), Eval) :-                     % Recursion-update state & continue
    updater(C, X, state(Acc, XPower), NewState),
    polynomial(Cs, X, NewState, Eval).

updater(Coeff, X, state(CurrAcc, CurrPow), state(NewAcc, NewPower)) :- % Update accumulator & X power
    NewAcc is CurrAcc + Coeff * CurrPow,
    NewPower is CurrPow * X.



%% A DAG is represented as a list of edges where an edge is
%% represented as a structure edge(From, To) representing an edge from
%% node From to node To.  

% #6: "15-points"
% dag_path(Dag, From, To, Path) will succeed if Path is a list of
% edges from node From to node To in DAG Dag.
% It should be assumed that Dag, From, To are ground (no variables).
%path(_Dag, _From, _To, _Path) :- 'TODO'.

%% A regex can be represented in Prolog as follows:
%%   + A Prolog `atomic` (atom or number) is a regex matching itself.
%%   + cclass(Atomics) is a regex matching any of the atomic elements in list
%%     Atomics. Normal regex notation: char class [abc].
%%   + If A and B are regex's, then so is alt(A, B) matching if either
%%     A or B match.  Normal regex notation: A|B.
%%   + If A is a regex, then so is rep(A) matching 0-or-more occurrences
%%     of what matches A.  Normal regex notation: A*.
%%   + If A, B, C, ... are regex's, then so is [A, B, C, ...] matching
%%     the sequence of what is matched by A followed by what matches B
%%     followed by what matches C, ... .  Normal regex notation: ABC...

path(_Dag, From, From, []).             % Empty path when starting node = dest

path(Dag, From, To, [Edge | Path]) :- 
    find_edge(Dag, From, Next, Edge),   % Find edge 'From' --> 'Next'
    path(Dag, Next, To, Path).          % Find path 'Next' to 'To' (recursive)

find_edge(Dag, From, Next, edge(From, Next)) :- 
    member(edge(From, Next), Dag).      % Check if edge exists



% #7: "25-points"
% re_match(Regex, InSeq): succeed iff regex Regexp matches list InSeq
% of atomics.
% *Hint*: atomic(X) succeeds iff X is a Prolog atomic.
% re_match(_Regex, _InSeq) :- 'TODO'.
		     
re_match(Regex, InSeq) :-                                              % Match entire list of atomics                                 
    res_match([Regex], InSeq, []).

res_match([], Atoms, Atoms).                                           % res_match(1, 2, 3) matches 1 against 2, leaving 3

res_match([Re | RestRes], [Atom | RestAtoms], Remaining) :-            % 1-atomic regex matches head of input seq            
    atomic(Re),                                                        % Ensure Re is atomic                                         
    atomic(Atom),                                                      % Ensure Atom is atomic               
    \+ Re \= Atom,                                                     % Re must match Atom                                                                       
    res_match(RestRes, RestAtoms, Remaining).

res_match([cclass(Atoms) | RestRes], [Atom | RestAtoms], Remaining) :- % 2-cclass([...]) matches any atom in the class
    member(Atom, Atoms),                                               % check for Atoms
    res_match(RestRes, RestAtoms, Remaining).

res_match([alt(Re1, _) | RestRes], InSeq, Remaining) :-                % 3-alt(A, B, ...) matches if any sub-regex matches        
    res_match([Re1 | RestRes], InSeq, Remaining).

res_match([alt(_, Re2) | RestRes], InSeq, Remaining) :-
    res_match([Re2 | RestRes], InSeq, Remaining).

res_match([rep(Re) | RestRes], InSeq, Remaining) :-                    % 4-rep(A) matches >= 0 A's        
    res_match([Re | [rep(Re) | RestRes]], InSeq, Remaining).

res_match([rep(_) | RestRes], InSeq, Remaining) :-
    res_match(RestRes, InSeq, Remaining).

res_match([[Re | SubRes] | RestRes], InSeq, Remaining) :-              % 5-seq of regex parts             
    res_match([Re | SubRes], InSeq, TempRemaining),                    
    res_match(RestRes, TempRemaining, Remaining).