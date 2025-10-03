% ---
% Solve the "Herbs" problem from the Coursera Course
% "Solving Algorithms for Discrete Optimization"
% by the The Chinese University of Hong Kong
% presented by Professors Peter James Stucky and Jimmy Ho Man Lee.
% at
% https://www.coursera.org/learn/solving-algorithms-discrete-optimization/lecture/0nWyf/3-1-5-search
%
% but:
%
% explicitly in Prolog instead of in MiniZinc.
%
% This code runs in SWI-Prolog 9.3.5.
%
% Problem statement:
%
% - There is a cupboard with 10 x 10 drawer matrix, each drawer is given a
%   coordinate from (1,1) to (10,10)
% - In each drawer, there a 5 types of seeds associated with 5 different elements.
%
% Shennong Shi goes through the drawers by increasing rows in an outer loop and
% increasing columns in an inner loop.
% He selects a seed from each drawer while upholding certain constraints.
% Once he has traversed all 100 drawers upheld all constraints, he has a valid
% seed set.
%
% This is finding a path through a tree of branching factor 5 and height 100 nodes.
%
% Contraints are as follows:
%
% - The elements of the seeds taken from adjacent drawers in the same row need to
%   obey certain generative or destructive relationships.
% - The seeds in the first drawer cannot be not be associated with metal.
% - Between 1 and 2 seeds in a column have to be associated with "metal".
% - Between 1 and 2 seeds in a column have to be associated with "earth".
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
% Available seeds (or rather, seed types). The
% special type 'none' is used in code but not mentioned here
% as it will never be selected.
% ---

seed(metal).
seed(wood).
seed(water).
seed(fire).
seed(earth).

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

search(100,_,_,Seeds,Seeds) :- !.

search(Pos,MetalColCountDict,EarthColCountDict,[PrevSeed|Seeds],Result) :-
   Pos < 100,
   % Row and Col go from 0 to 9.
   divmod(Pos,10,Row,Col),                                                             
   % Non-deterministically choose a seed (identified with one of the 5 characteristics).
   seed(CurSeed),                                                                        
   % Constraint
   no_metal_on_position_zero(Pos,CurSeed),
   % Constraint
   next_seed_relationship_is_good(Row,Col,PrevSeed,CurSeed),
   % Inform the user once we have passed the first constrain above
   format("Trying ~|~32t~a~32t~5+ for Pos=~d, Row=~d, Col=~d~n",[CurSeed,Pos,Row,Col]),
   metal_count_per_column_is_good(Row,Col,CurSeed,MetalColCountDict),
   % format("metal_count_per_column_is_good passed for row=~d,col=~d,seed=~w~n",[Row,Col,Seed]),
   earth_count_per_column_is_good(Row,Col,CurSeed,EarthColCountDict),
   % format("earth_count_per_column_is_good passed for row=~d,col=~d,seed=~w~n",[Row,Col,Seed]),
   (
      CurSeed==metal 
      ->
      inc_col_count(MetalColCountDict,Col,MetalColCountDictNext)
      ;
      MetalColCountDictNext = MetalColCountDict
   ),
   (
      CurSeed==earth
      ->
      inc_col_count(EarthColCountDict,Col,EarthColCountDictNext)
      ;
      EarthColCountDictNext = EarthColCountDict
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
% Kick off the search, then print the results.
% ---

main :-
   search(0,_{},_{},[none],ReverseSeeds),
   reverse(ReverseSeeds,[none|Seeds]),
   display(Seeds,0).
 
