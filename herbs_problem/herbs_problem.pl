% ---
% Solve the "Herbs" problem from the Coursera Course
% "Solving Algorithms for Discrete Optimization"
% by the The Chinese University of Hong Kong
% presented by Professors Peter James Stucky and Jimmy Ho Man Lee.
% at
% https://www.coursera.org/learn/solving-algorithms-discrete-optimization/lecture/0nWyf/3-1-5-search
% ---

% ---
% In the course, the problem is solved using MiniZinc, here we solve it with
% SWI-Prolog, which is not as declarative, but still quite instructive.
%
% This code runs in SWI-Prolog 9.3.5.
%
% The CLP(FD) (constraint logic programming over finite domains) is
% used for the #=/2 (integer equal) predicate.
%
% We will be using the CLP(FD) predicate "#=" for integer equality
% rather than the less declarative predicate "is/2" or "=:=/2"
% ---

:- use_module(library(clpfd)).
:- use_module('modules/extract_row_or_col.pl').
:- use_module('modules/verify_herbs_solution.pl').
:- use_module('modules/display_herbs_solution.pl').

% ---
% Available seeds (or rather, seed types). The
% special type 'none' is used in code but not mentioned here
% as it will never be selected. 
%
% Calling seed(X) will instantiate X successively with 
% metal, wood, water, fire, earth in that order. So we and
% do backtracking over the possibilities by backtracking
% into seed(X).
% ---

seed(metal).
seed(wood).
seed(water).
seed(fire).
seed(earth).

% ---
% Another scheme to generate the elements. In this case, the
% elements are returned according to a random permutation (computed
% anew for each node) This makes search more interesting.
% ---

seed_random(Element) :-
   random_permutation([metal,wood,water,fire,earth],Perm),
   !, % when backtracking, do not go back to generating another permutation
   member(Element,Perm). % this will set Element successively to the values of the permutation

% ---
% The generative and destructive (non-symmetric) relationships between elements
% ---

generative(water,wood).
generative(wood,fire).
generative(fire,earth).
generative(earth,metal).
generative(metal,water).

destructive(water,fire).
destructive(fire,metal).
destructive(metal,wood).
destructive(wood,earth).
destructive(earth,water).

% ---
% Helper to increase the per-col count of for an element associated to a seed
% (for either metal or earth. Here, SWI-Prolog "dicts" are used.
% ---

inc_col_count(DictIn,Col,DictOut) :-
   get_dict(Col,DictIn,CurCount),          % on success, key "Col" exists, so we have seen at least 1
   !,                                      % commit to the branch in which we have seen at least one
   assertion(CurCount > 0),
   CurCount < 2,                           % fail immediately if CurCount is already 2, signalling "no can do"
   succ(CurCount,NextCount),               % otherwise build next dict with count for column increased by 1
   put_dict(Col,DictIn,NextCount,DictOut).

inc_col_count(DictIn,Col,DictOut) :-
   \+ get_dict(Col,DictIn,_),              % failure means key does not exist (yet) (this line is superfluous)
   put_dict(Col,DictIn,1,DictOut).         % we have seen one

% ---
% Retrieving the column count
% ---

col_count(DictIn,Col,N) :-
   get_dict(Col,DictIn,N),!.               % get the value, which exists if get_dict/3 succeeds

col_count(_DictIn,_Col,0).                 % if the previous clause failed, assume 0.

% ---
%% no_metal_on_position_zero(+Pos,+Seed)
%
% Choose the non-logical way of expressing this, using cuts.
% ---

no_metal_on_position_zero(0,metal) :- !,false. % commit then fail
no_metal_on_position_zero(_,_).                % by default true

% ---
%% metal_count_per_column_is_good(+Row,+Col,+CurSeed,+PerColCountingDict)
%
% Between 1 and 2 seeds in any column have to be associated with "metal" seed.
% ---

% we have selected "metal" on any row
metal_count_per_column_is_good(_Row,Col,metal,ColCountDict) :- !,col_count(ColCountDict,Col,N),between(0,1,N).

% we have selected "not metal" on row 9
metal_count_per_column_is_good(9,Col,Seed,ColCountDict) :- Seed\=metal,!,col_count(ColCountDict,Col,N),between(1,2,N).

% we have selected "not metal" on row <9
metal_count_per_column_is_good(Row,Col,Seed,ColCountDict) :- Seed\=metal,Row<9,!,col_count(ColCountDict,Col,N),between(0,2,N).

% ---
%% earth_count_per_column_is_good(+Row,+Col,+CurSeed,+PerColCountingDict)
%
% Between 1 and 2 seeds in any column have to be associated with "earth" seed.
% ---

% we have selected "earth" on any row
earth_count_per_column_is_good(_Row,Col,earth,ColCountDict) :- !,col_count(ColCountDict,Col,N),between(0,1,N).

% we have selected "not earth" on row 9
earth_count_per_column_is_good(9,Col,Seed,ColCountDict) :- Seed\=earth,!,col_count(ColCountDict,Col,N),between(1,2,N).

% we have selected "not earth" on row <9
earth_count_per_column_is_good(Row,Col,Seed,ColCountDict) :- Seed\=earth,Row<9,!,col_count(ColCountDict,Col,N),between(0,2,N).

% ---
%% next_seed_relationship_is_good(+Row,+Col,+PrevSeed,CurSeed)
% ---

next_seed_relationship_is_good(0,0,_,_) :- !.                  % no relationship on first seed
next_seed_relationship_is_good(_,0,_,_) :- !.                  % no relationship on column 0.
next_seed_relationship_is_good(_,_,PrevSeed,CurSeed) :-        % otherwise disjunction
    generative(PrevSeed,CurSeed);destructive(PrevSeed,CurSeed).

% ---
%% search(+Pos,+MetalColCountDict,+EarthColCountDict,+GrowingListOfSeeds,-FinalListOfSeeds)
% ---

search(100,_,_,Seeds,Seeds) :- !.

search(Pos,MetalColCountDict,EarthColCountDict,[PrevSeed|Seeds],Result) :-
   Pos < 100,
   % Row and Col go from 0 to 9.
   % Col changes fastest (is the 2nd dimension)
   % Row changes slowest (is the 1st dimension)
   divmod(Pos,10,Row,Col),

   % Non-deterministically choose a seed. 
   % (directly identified with one of the 5 characteristics 
   % metal, wood, water, fire, earth.)
   % We provide two ways of choosing the seed: sequential ("seq") and 
   % randomly ("rand").
   % Note that there is no restriction on the seed proposed by
   % seed/1, we just blindly propose and then test whether the
   % constraints hold. For more efficiency, one would probably
   % provide more info to the seed/N predicate so that it can
   % make more informed proposal.
   (proposal(seq) -> seed(CurSeed) ; seed_random(CurSeed)),

   % Constraint on first seed (it would be more efficient to make the decision for the
   % constraint-passing first seed before recursion is started)
   no_metal_on_position_zero(Pos,CurSeed),
   % Constraint on relationship to previous seed, generative or destructive.
   next_seed_relationship_is_good(Row,Col,PrevSeed,CurSeed),
   % Inform the user once we have passed the first constrain above.
   format("Trying ~|~32t~a~32t~5+ for Pos=~d, Row=~d, Col=~d~n",[CurSeed,Pos,Row,Col]),
   metal_count_per_column_is_good(Row,Col,CurSeed,MetalColCountDict),
   % format("metal_count_per_column_is_good passed for row=~d,col=~d,seed=~w~n",[Row,Col,Seed]),
   earth_count_per_column_is_good(Row,Col,CurSeed,EarthColCountDict),
   % format("earth_count_per_column_is_good passed for row=~d,col=~d,seed=~w~n",[Row,Col,Seed]),
   (
      CurSeed==metal
      ->
      inc_col_count(MetalColCountDict,Col,MetalColCountDictNext) % build new dict
      ;
      MetalColCountDictNext = MetalColCountDict % use existing dict
   ),
   (
      CurSeed==earth
      ->
      inc_col_count(EarthColCountDict,Col,EarthColCountDictNext) % build new dict
      ;
      EarthColCountDictNext = EarthColCountDict % use existing dict
   ),
   succ(Pos,PosNext),
   % Recursive call follows
   search(PosNext,MetalColCountDictNext,EarthColCountDictNext,[CurSeed,PrevSeed|Seeds],Result).

% ---
% How the seeds are proposed during search: "sequentially" or "at random".
% This can be changed on the Prolog command line by issuing:
% ?- set_proposal(rand).
% ?- set_proposal(seq).
% To query current setting:
% ?- proposal(X). 
% ---

:- dynamic proposal/1. % proposal/1 can be modified at runtime

set_proposal(What) :-
   assertion((atom(What),member(What,[seq,rand]))),
   retractall(proposal(_)),
   asserta(proposal(What)).

:- set_proposal(seq). % initial setting

% ---
% Kick off the search, then print the results.
% How to run it on the Prolog command line:
% ?- ['herbs_problem.pl'].
% ?- main.
% ---

main :-
   % Start search:
   % - At position 0
   % - With two empty dicts that will grow to handle the 
   %   mappings "col -> #earth", "col -> #metal"
   % - With the growing list of elements (aka seeds) starting
   %   as the list [none] to have an unused "previous seed" at
   %   position 0.
   %   This list grows at its front (by prepending), not at its back,
   %   so it has to be reversed once we are done.
   % - With the var "ReverseSeeds" unified with the final list
   %   of elements at recursion end (i.e. at position 100).
   search(0,_{},_{},[none],ReverseSeeds),
   % Drop the artificial 'none' entry when reversing.
   reverse(ReverseSeeds,[none|Seeds]),
   display(Seeds,0),
   assertion(verify(Seeds)).
