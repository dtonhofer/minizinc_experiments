% ---
% Solve the "Herbs" problem from the Coursera Course
% "Solving Algorithms for Discrete Optimization"
% by the The Chinese University of Hong Kong
% presented by Professors Peter James Stucky and Jimmy Ho Man Lee.
% at
% https://www.coursera.org/learn/solving-algorithms-discrete-optimization/lecture/0nWyf/3-1-5-search
%
% but explicitly in Prolog instead of MiniZinc.
%
% This code runs in SWI-Prolog 9.3.5.
% The CLP(FD) (constraint logic programming over finite domains) is
% used for the #=/2 (integer equal) predicate.
%
% Problem statement
% -----------------
%
% The problem describes a search tree of depth 100 and constant branching factor 5.
%
% - There is a cupboard with 10 x 10 drawers, traversed by row first, column second,
%   defining 100 places.
%   Each drawer is given a coordinate from (1,1) to (10,10) in the course material.
%   We use (0,0) to (9,9) instead.
%
% - In each drawer, there a 5 types of seeds associated with 5 different elements,
%   metal, woof, water, fire, earth.
%
% - Shennong Shi goes through the drawers by row first, column second.
%
% - He selects a seed from the 5 in each each drawer while upholding certain constraints
%   among the seeds already selected and the new seed.
%
% - Once he has traversed all 100 drawers and upheld all constraints, he has a valid
%   seed sequence.
%
% Constraints are as follows:
%
% - The elements of the seeds selected from adjacent drawers (a drawer and its successor
%   drawer) *in the same row* must obey either a "generative" or a "destructive"
%   relationships (freely selected), as follows:
%
%   generative             destructive
%   ----------             -----------
%   water -> wood          water -> fire
%   wood  -> fire          fire -> metal
%   fire  -> earth         metal -> wood
%   earth -> metal         wood -> earth
%   metal -> water         earth -> water
%
% - The seed from the first drawer cannot be associated with "metal".
%
% - In each column, between 1 and 2 seeds must be associated with "metal".
%
% - In each column, between 1 and 2 seeds must be associated with "earth".
%
% A possible solution:
%
%   wood   ,   fire   ,  metal   ,   wood   ,   fire   ,  metal   ,   wood   ,   fire   ,  metal   ,   wood
%  metal   ,   wood   ,   fire   ,  metal   ,   wood   ,   fire   ,  metal   ,   wood   ,   fire   ,  metal
%  metal   ,   wood   ,   fire   ,  metal   ,   wood   ,   fire   ,  metal   ,   wood   ,   fire   ,  metal
%   wood   ,   fire   ,  metal   ,   wood   ,   fire   ,  metal   ,   wood   ,   fire   ,  metal   ,   wood
%   wood   ,   fire   ,  earth   ,  water   ,   wood   ,   fire   ,  earth   ,  metal   ,   wood   ,   fire
%   wood   ,   fire   ,  earth   ,  water   ,   wood   ,   fire   ,  earth   ,  metal   ,   wood   ,   fire
%   wood   ,  earth   ,  water   ,   wood   ,   fire   ,  earth   ,  water   ,   wood   ,   fire   ,  earth
%   wood   ,  earth   ,  water   ,   wood   ,   fire   ,  earth   ,  water   ,   wood   ,   fire   ,  earth
%  water   ,   wood   ,   fire   ,  earth   ,  metal   ,   wood   ,   fire   ,  earth   ,  water   ,   wood
%  earth   ,  metal   ,   wood   ,   fire   ,  earth   ,  water   ,   wood   ,   fire   ,  earth   ,  water
% ---

% We will be using the CLP(FD) predicate "#=" for integer equality
% rather than the nasties "is/2" and "=:=/2"

:- use_module(library(clpfd)).

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

% Another scheme to generate the elements. In this case, the
% elements are returned according to a random permutation (computed
% anew for each node) This makes search more interesting.

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

% ===
% Constraints
% ===

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
% The path through the search space is given by a reverse list of
% seeds selected from each drawer. The position goes from 0 to 99.
% ---

sequential_proposal :- true. % set to false for "random proposal"

search(100,_,_,Seeds,Seeds) :- !.

search(Pos,MetalColCountDict,EarthColCountDict,[PrevSeed|Seeds],Result) :-
   Pos < 100,
   % Row and Col go from 0 to 9.
   divmod(Pos,10,Row,Col),

   % Non-deterministically choose a seed. 
   % (directly identified with one of the 5 characteristics 
   % metal, wood, water, fire, earth.)
   % We provide two ways of choosing the seed.
   % Note that there is no restriction on the seed proposed by
   % seed/1, we just blindly propose and then test whether the
   % constraints hold. For more efficiency, one would probably
   % provide more info to the seed/N predicate so that it can
   % make more informed proposal.

   (sequential_proposal -> seed(CurSeed) ; seed_random(CurSeed)),

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
% Printing results
% ---

display([],100).
display([S|Seeds],Pos) :-
   assertion(Pos < 100),
   divmod(Pos,10,_,Col),
   format("~|~32t~a~32t~10+",[S]),
   ((Col == 9) -> format("~n",[]) ; format(",",[])),
   succ(Pos,PosNext),
   display(Seeds,PosNext).

% ---
% Verification of result
% ---

extract_col(Col,Seeds,ColEntries) :-
   bagof(Element,
         Row^Index^(
            between(0,9,Row),
            Index #= Col+Row*10,
            nth0(Index,Seeds,Element)
         ),
         ColEntries),
   % format("Extracted col ~d is ~w~n",[Col,ColEntries]),
   true. % only exists to make the previous line out-commentable

extract_row(Row,Seeds,RowEntries) :-
   bagof(Element,
         Col^Index^(
            between(0,9,Col),
            Index #= Col+Row*10,
            nth0(Index,Seeds,Element)
         ),
         RowEntries),
   % format("Extracted row ~d is ~w~n",[Row,RowEntries]),
   true. % only exists to make the previous line out-commentable

verify_col(Col,ChosenElement,Seeds) :-
   extract_col(Col,Seeds,ColEntries),
   aggregate(count, (member(X, ColEntries), X == ChosenElement), Count),
   % format("Count for column ~d and element ~w is ~d~n",[Col,ChosenElement,Count]),
   between(1,2,Count).

verify_row_successor_relationship([]).
verify_row_successor_relationship([_]).
verify_row_successor_relationship([X,Y|Rest]) :-
   (generative(X,Y);destructive(X,Y)),
   verify_row_successor_relationship([Y|Rest]).

verify_row(Row,Seeds) :-
   extract_row(Row,Seeds,RowEntries),
   verify_row_successor_relationship(RowEntries).
   
verify(Seeds) :-
   (nth0(0,Seeds,metal) -> false ; true),
   % format("First seed is not 'metal'~n"),
   numlist(0, 9, Indexes),
   forall(member(Row,Indexes),verify_row(Row,Seeds)),
   % format("Rows are good~n"),
   forall(member(Col,Indexes),verify_col(Col,metal,Seeds)),
   % format("Cols are good for 'metal'~n"),
   forall(member(Col,Indexes),verify_col(Col,earth,Seeds)),
   % format("Cols are good for 'earth'~n"),
   true. % only exists to make the previous line out-commentable

% ---
% Kick off the search, then print the results.
% ---

main :-
   search(0,_{},_{},[none],ReverseSeeds),
   reverse(ReverseSeeds,[none|Seeds]), % drop the artificial 'none' entry
   display(Seeds,0),
   assertion(verify(Seeds)).
