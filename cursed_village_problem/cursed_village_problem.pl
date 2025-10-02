% ---
% Solve the "Cursed Village" / "Sick Children" / "Constellations and Baguas" 
% problem from the Coursera Course "Solving Algorithms for Discrete Optimization"
% https://www.coursera.org/learn/solving-algorithms-discrete-optimization/lecture/hCiWy/3-1-4-propagation-engine
% by the The Chinese University of Hong Kong
% presented by Professors Peter James Stucky and Jimmy Ho Man Lee.
%
% but:
%
% explicitly in Prolog instead of in MiniZinc. In the latter one would
% specify a high-level description of the constraints. Raw Prolog demands
% more detailed management closely aligned with the ideas presented while
% presenting the problem in the vide "3.1.4: Propagation Engine".
%
% This code runs in SWI-Prolog 9.3.5.
%
% Problem statement:
%
% - We have 8 bagua powers.
% - We have 28 constellation.
% - Each constellation is associated with a nonempty set of bagua powers
%   i.e. is associated to an element of the powerset of bagua powers that is
%   not the empty set.
% - We have the healer Shennongshi who can actually remove bagua powers
%   from the constellations.
% - We have 25 male children and 10 female children.
% - Each child is associated to two constellations, labeled the child's
%   "major constellation" and the child's "minor constellation".
%   This means that the child is associated to two set of bagua powers,
%   which we can call his/her "major baguas power" and "minor baguas powers". 
% - A child is at risk of becoming sick if:
%   - It is a male and there is a pair of bagua powers (a,b)
%     where "a" has been chosen from the child's "major bagua powers"
%     and "b" has been chosen from the child's "minor bagua powers"
%     such that (a,b) NOT appear in a fixed set of allowed pairs called the
%     "book of changes".
%   - It is a female and there is a bagua power "c" which appears in
%     both her "major bagua powers" and "minor bagua powers" (i.e. the
%     the major and minor bagua powers do not form disjoint sets).
% - Task: Remove bagua powers from the constellations (whithout 
%   going so far as getting the empty set) so that none of the children
%   are at risk of becoming sick. A resulting association of 
%   constellations to sets of bagua power sets obtained from the original
%   bagua power sets by element removal is a solution.
%
% The first solution output is this one, more can be found by backtracking:
%
% ape : [marsh,mountain,water]
% bat : [earth,fire,heaven,marsh,mountain,wind]
% bird : [earth]
% chicken : [earth,heaven,mountain]
% chinese_unicorn : [heaven,mountain,thunder,wind]
% cow : [fire,thunder]
% deer : [fire,marsh,mountain]
% devil : [wind]
% dog : [mountain]
% earth_worm : [fire,heaven,marsh]
% flood_dragon : [earth,fire,marsh,water]
% fox : [wind]
% gold_dragon : [wind]
% hare : [fire,marsh,mountain,thunder,wind]
% horse : [fire,heaven,marsh,thunder]
% leopard : [earth,heaven,marsh,wind]
% monkey : [mountain]
% pheasant : [marsh,water,wind]
% pig : [marsh,thunder]
% raccoon : [fire,heaven,marsh,thunder]
% rat : [heaven]
% roe_deer : [thunder]
% sheep : [earth,heaven,thunder,wind]
% snake : [earth,water]
% sparrow : [wind]
% tiger : [fire,heaven,marsh]
% wild_dog : [earth]
% wolf : [mountain,thunder]
% ---

:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(ordsets)).

% ---
% Two sexes. Defining the 'sex' atoms here allows us to
% backtrack over them or check syntax if needed.
% ---

sex(male).
sex(female).

% ---
% Eight baguas. Defining the 'bagua' atoms here allows us to
% backtrack over them or check syntax if needed.
% ---

bagua(heaven).
bagua(marsh).
bagua(fire).
bagua(thunder).
bagua(wind).
bagua(water).
bagua(mountain).
bagua(earth).

print_number_of_baguas :-
   bagof(X,bagua(X),Bag),
   length(Bag,Len),
   format("There are ~d baguas~n",[Len]).

% ---
% Twenty-eight constellations. Defining the 'constellation' atoms here
% allows us to backtrack over them or check syntax if needed.
% ---

constellation(flood_dragon).
constellation(gold_dragon).
constellation(raccoon).
constellation(hare).
constellation(fox).
constellation(tiger).
constellation(leopard).
constellation(wild_dog).
constellation(sheep).
constellation(roe_deer).
constellation(horse).
constellation(deer).
constellation(snake).
constellation(earth_worm).
constellation(wolf).
constellation(dog).
constellation(pheasant).
constellation(chicken).
constellation(bird).
constellation(monkey).
constellation(ape).
constellation(chinese_unicorn).
constellation(cow).
constellation(bat).
constellation(rat).
constellation(sparrow).
constellation(pig).
constellation(devil).

print_number_of_constellations :-
   bagof(X,constellation(X),Bag),
   length(Bag,Len),
   format("There are ~d constellations~n",[Len]).

% ---
% "Book of changes" lists allowed "bagua pairs" for male children.
% Each child is associated to two constellations, which are 
% associated to a nonempty set of baguas, and for male children
% we demand that any ordered pair of baguas selected from the 
% tow constellations in any order appear in the "book of changes.
% ---

book_of_changes(heaven,heaven).
book_of_changes(earth,earth).
book_of_changes(earth,wind).
book_of_changes(water,earth).
book_of_changes(water,wind).
book_of_changes(water,marsh).
book_of_changes(fire,heaven).
book_of_changes(fire,thunder). % these two are 
book_of_changes(thunder,fire). % symmetric!
book_of_changes(wind,thunder).
book_of_changes(wind,mountain).
book_of_changes(mountain,thunder).
book_of_changes(marsh,heaven).
book_of_changes(marsh,fire).
book_of_changes(marsh,marsh).

verify_book_of_changes :-
   % The book_of_changes/2 predicate uses only valid bagua atoms.
   forall(book_of_changes(B1,B2),(atom(B1),atom(B2),bagua(B1),bagua(B2))),
   % Create a bag of the pairs; will fail if there are no solution to book_of_changes(A.B).
   % The term B1-B2 is just the bagua atoms connected by '-'.
   bagof(B1-B2,book_of_changes(B1,B2),Bag),
   % The bag of the pairs contains no duplicates.
   is_set(Bag).

print_number_of_book_of_changes_entries :-
   bagof(B1-B2,book_of_changes(B1,B2),Bag),
   length(Bag,Len),
   format("There are ~d entries in 'book_of_changes/2'~n",[Len]).

% ---
% The mapping of constellation to their inital 'sets of baguas' (technically, 'lists of baguas').
% ---

constellation_baguas(flood_dragon,    [ earth, mountain, water, wind, thunder, fire, marsh ]).
constellation_baguas(gold_dragon,     [ earth, water, wind, thunder, marsh, heaven ]).
constellation_baguas(raccoon,         [ earth, mountain, water, wind, thunder, fire, marsh, heaven ]).
constellation_baguas(hare,            [ earth, mountain, water, wind, thunder, fire, marsh, heaven ]).
constellation_baguas(fox,             [ earth, mountain, water, wind, fire, marsh, heaven ]).
constellation_baguas(tiger,           [ earth, mountain, water, wind, thunder, fire, marsh, heaven ]).
constellation_baguas(leopard,         [ earth, water, wind, thunder, marsh, heaven ]).
constellation_baguas(wild_dog,        [ earth, mountain, water, fire, marsh, heaven ]).
constellation_baguas(sheep,           [ earth, mountain, water, wind, thunder, fire, marsh, heaven ]).
constellation_baguas(roe_deer,        [ earth, mountain, water, wind, thunder, fire, marsh, heaven ]).
constellation_baguas(horse,           [ earth, mountain, water, wind, thunder, fire, marsh, heaven ]).
constellation_baguas(deer,            [ mountain, thunder, fire, marsh, heaven ]).
constellation_baguas(snake,           [ earth, mountain, water, wind, fire, marsh, heaven ]).
constellation_baguas(earth_worm,      [ earth, thunder, fire, marsh, heaven ]).
constellation_baguas(wolf,            [ earth, mountain, wind, thunder, fire, marsh, heaven ]).
constellation_baguas(dog,             [ mountain, water, thunder, fire, marsh, heaven ]).
constellation_baguas(pheasant,        [ earth, mountain, water, wind, thunder, marsh ]).
constellation_baguas(chicken,         [ earth, mountain, heaven ]).
constellation_baguas(bird,            [ earth, mountain, water ]).
constellation_baguas(monkey,          [ earth, mountain, wind, fire, marsh, heaven ]).
constellation_baguas(ape,             [ earth, mountain, water, wind, thunder, marsh, heaven ]).
constellation_baguas(chinese_unicorn, [ mountain, wind, thunder, fire, marsh, heaven ]).
constellation_baguas(cow,             [ earth, water, wind, thunder, fire, marsh, heaven ]).
constellation_baguas(bat,             [ earth, mountain, water, wind, fire, marsh, heaven ]).
constellation_baguas(rat,             [ water, wind, thunder, fire, marsh, heaven ]).
constellation_baguas(sparrow,         [ earth, wind, thunder, fire, marsh, heaven ]).
constellation_baguas(pig,             [ earth, water, wind, thunder, marsh ]).
constellation_baguas(devil,           [ earth, wind, fire, marsh, heaven ]).

verify_is_nonempty_set_of_baguas(_,Baguas) :-
   is_list(Baguas),
   Baguas \= [],
   % format("The list-of-baguas for constellation '~w' are a nonempty list~n",[Constel]),
   is_set(Baguas),
   % format("The list-of-baguas for constellation '~w' form a nonempty 'set'~n",[Constel]),
   maplist([B]>>(atom(B),bagua(B)),Baguas),
   % format("The list-of-baguas of constellation '~w' contains only valid bagua atoms~n",[Constel]),
   true. % NOP, only exist to able to easily comment-out the previous line

verify_every_constellation_appears_exactly_once_in_constellation_baguas :-
   bagof(C,Bs^constellation_baguas(C,Bs),Constels), % Bs must stay a free variable.
   is_set(Constels),                                   % Every element appears once only.
   % format("Every constellation appears at most once as first argument in constellation_baguas/2~n"),
   bagof(C,constellation(C),DefConstels),              % Collect constellation definitions.
   sort(Constels,ConstelsSorted),                      % Sort first args of constellation_baguas/2.
   sort(DefConstels,DefConstelsSorted),                % Sort single args of constellation/1.
   ConstelsSorted == DefConstelsSorted,                % Must be the same.
   format("Every constellation declared appears as first argument in constellation_baguas/2~n"),
   true. % NOP, only exist to able to easily comment-out the previous line
 
verify_constellation_baguas :- 
   % The first argument to constellation_baguas/2 is a 'constellation' atom.
   % The second argument to constellation_baguas/2 is a nonempty set of 'bagua' atoms.
   forall(
      constellation_baguas(Constel,Baguas),
      (
        atom(Constel),
        constellation(Constel),
        verify_is_nonempty_set_of_baguas(Constel,Baguas)
      )
   ),
   verify_every_constellation_appears_exactly_once_in_constellation_baguas.

% ---
% child(+MajConstel, +MinConstel, +Sex).
% Male children of the village. They set up a constraint between the
% sets of baguas attached to the major and minor constellations.
% ---

child(flood_dragon, leopard, male).
child(gold_dragon, wolf, male).
child(raccoon, horse, male).
child(hare, cow, male).
child(fox, wolf, male).
child(tiger, rat, male).
child(leopard, chinese_unicorn, male).
child(leopard, sheep, male).
child(wild_dog, bird, male).
child(horse, tiger, male).
child(deer, pig, male).
child(snake, wild_dog, male).
child(snake, fox, male).
child(earth_worm, raccoon, male).
child(earth_worm, rat, male).
child(wolf, cow, male).
child(dog, roe_deer, male).
child(pheasant, deer, male).
child(pheasant, bat, male).
child(chicken, sheep, male).
child(bird, gold_dragon, male).
child(ape, pig, male).
child(sparrow, roe_deer, male).
child(devil, monkey, male).
child(devil, dog, male).

% ---
% child(+MajConstel, +MinConstel, +Sex).
% Female children of the village. They set up a constraint between the
% sets of baguas attached to the major and minor constellations.
% ---

child(flood_dragon, chinese_unicorn, female).
child(hare, rat, female).
child(fox, bird, female).
child(tiger, monkey, female).
child(wild_dog, wolf, female).
child(sheep, dog, female).
child(roe_deer, pheasant, female).
child(deer, sparrow, female).
child(wolf, wild_dog, female).
child(ape, devil, female).

verify_children :- 
   forall(
      child(MinConstel,MajConstel,Sex),
      (
         atom(MinConstel),
         atom(MajConstel),
         atom(Sex),
         constellation(MinConstel),
         constellation(MajConstel),
         sex(Sex),
         MinConstel \= MajConstel
      )
   ),
   bagof([MaC,MiC,Sex],child(MaC,MiC,Sex),Children),
   is_set(Children).   

% ---
% To verify the database
% ---

verify_database :-
   print_number_of_baguas,
   print_number_of_constellations,
   print_number_of_book_of_changes_entries,
   verify_book_of_changes,
   verify_constellation_baguas,
   verify_children.

% ---
% Loading database data into on-stack structures,
%  a dict for the "constellation->ordset-of-baguas" mapping
%  a list for the children
% ---

load_constellation_to_bagua_set_mapping_into_dict(Dict) :-
   findall(
      Constel:BaguaOrdSet, % This is just the term :(Constel,BaguaOrdSet)
      (constellation_baguas(Constel,Baguas),list_to_ord_set(Baguas,BaguaOrdSet)),
      Data
   ),
   dict_create(Dict,ctb,Data). % "ctb" is an arbitrary tag of the Dict

load_children_into_list(Children) :-
   findall(child(C1,C2,male), child(C1,C2,male), Males),
   findall(child(C1,C2,female), child(C1,C2,female), Females),
   length(Males,ML),
   format("There are ~d male children~n",[ML]),
   length(Females,FL),
   format("There are ~d female children~n",[FL]),
   append([Males,Females],Children).

% ---
% Printing the "constellation->ordset-of-baguas" mapping finally obtained
% ---

printout_for_constellation(C,ConstelDict) :-
   get_dict(C,ConstelDict,BaguaOrdSet),
   format("~w : ~w~n", [C,BaguaOrdSet]).

printout(ConstelDict) :- 
   bagof(C,constellation(C),DefConstels),      % Collect constellation definitions.
   sort(DefConstels,DefSortedConstels),        % Sort single args of constellation/1.
   forall(
      member(C,DefSortedConstels),
      printout_for_constellation(C,ConstelDict)
   ).


% ---
%% is_safe_2(+Sex,+BaguaSet1,+BaguaSet2)
%
% "BaguaSet1" is the bagua set (an ordered set implemented as list)
%             corresponding to the MAJOR constellation C1.
% "BaguaSet2" is the bagua set corresponding to the
%             MINOR constellation C2.
% ---

% For females, the major and minor sets of baguas must be disjoint.

is_safe_2(female,BaguaSet1,BaguaSet2) :-
   ord_disjoint(BaguaSet1,BaguaSet2).

% For males, all the possible ordered pairs 
% (major constellation bagua, minor constellation bagua)
% must be in the book of changes. 
% The specification is unclear, but in the book of changes, the first position
% of a pair always corresponds to the MAJOR constellation (the one associated
% to BaguaSet1) and the second position of a pair always corresponds to the
% MINOR constellation (the one associated to BaguaSet2).

is_safe_2(male,BaguaSet1,BaguaSet2) :-
   forall(
      member(B1,BaguaSet1),         % for all B1 from BaguaSet1
      (
         book_of_changes(B1,B2),    % there is at least one book of changes entry (B1,B2)
         member(B2,BaguaSet2)       % with B2 from BaguaSet2
      )
   ),
   forall(
      member(B2,BaguaSet2),         % for all B2 from BaguaSet2
      (
         book_of_changes(B1,B2),    % there is at least one book of changes entry (B1,B2)
         member(B1,BaguaSet1)       % with B1 from BaguaSet1
      )
   ).

% ---
%% is_safe(+C1,+C2,+Sex,+ConstelDict)
% Determine whether a child is SAFE under the current "ConstelDict".
% C1 is the "major constellation"
% C2 is the "minor constellation"
% ---

is_safe(C1,C2,Sex,ConstelDict) :-
   get_dict(C1,ConstelDict,BaguaSet1),
   get_dict(C2,ConstelDict,BaguaSet2),
   is_safe_2(Sex,BaguaSet1,BaguaSet2).

% ---
%% collect_males_made_unsafe(
%%    +ListOfImplicatedConstellations,  input
%%    +ListOfSafeMalesIn,               input  
%%    +ConstelDict,                     may or may not be used, see below
%%    -NewlyUnsafeMels,                 output
%%    -StillSafeMels)                   output
%
% There are two ways of writing this:
%
% Approximate: Grab all males that have one of the constellations
%              with changed baguas. This may collect males that
%              are actually still SAFE.
% Precise:     Grab only the males that are now actually
%              UNSAFE according to is_safe/4, i.e. under the new
%              "ConstelDict"
%
% We choose the approximate way (faster).
% ---

% No constellation is implicated in bagua removal, there
% is actually nothing to do then as ConstelDict did not change.

collect_males_made_unsafe([],SMels,[],SMels).

% A single constellation is implicated (i.e. the other
% constellation did not have its bagua set reduced)

collect_males_made_unsafe([Cx],SMels,NewlyUnsafeMels,StillSafeMels) :-
   partition(
      {Cx}/[child(C1,C2,male)]>>(Cx==C1;Cx==C2),
      SMels,NewlyUnsafeMels,StillSafeMels
   ).

% Two constellations are implicated (i.e. both constellation
% had their bagua sets reduced)

collect_males_made_unsafe([Cx1,Cx2],SMels,NewlyUnsafeMels,StillSafeMels) :-
   partition(
      {Cx1,Cx2}/[child(C1,C2,male)]>>(Cx1==C1;Cx1==C2;Cx2==C1;Cx2==C2),
      SMels,NewlyUnsafeMels,StillSafeMels
   ).

% ---
% Transform the result of a predicate call (goal) into one of the atoms 'true', 'false'
% ---

reify(Goal, Result) :-
    (   call(Goal)
    ->  Result = true
    ;   Result = false
    ).

% ---
% Adding all the baguas in an "Intersection" set to one or the other of two existing sets, 
% so that the intersection of the extended sets stays disjoint.
% ---

distribute2(first,I,Set1,Set2,[I|Set1],Set2).
distribute2(second,I,Set1,Set2,Set1,[I|Set2]).
distribute2(none,_,Set1,Set2,Set1,Set2).
      
distribute(Set1,Set2,[],Set1Out,Set2Out) :-
   Set1 \= [], % can't accept an empty set of baguas, backtrack
   Set2 \= [], % can't accept an empty set of baguas, backtrack
   list_to_ord_set(Set1,Set1Out),
   list_to_ord_set(Set2,Set2Out).

distribute(Set1,Set2,[I|Is],ExtendedSet1,ExtendedSet2) :-
   member(Choice,[first,second,none]), % backtackable
   distribute2(Choice,I,Set1,Set2,Set1Next,Set2Next),
   distribute(Set1Next,Set2Next,Is,ExtendedSet1,ExtendedSet2).

% ---
%% domain_propagation_for_female(+Set1,+Set2,-ReducedSet1,-ReducedSet2)
%
% Make two bagua sets (which are ordered sets implemented as lists) disjoint.
% This is a non-deterministic operation, i.e. we have to make choices, but we can
% backtrack later over the decisions if we find that no solution results after
% making those choices. The predicate that does that is distribute/4.
% ---

domain_propagation_for_female(Set1,Set2,ReducedSet1,ReducedSet2) :-
   intersection(Set1,Set2,Intersection),
   assertion(Intersection\=[]), % If the female child is UNSAFE, the intersection cannot be empty!
   % Any element in "Intersection" can be assigned to one or to the other or to none of the
   % output BaguaOrdSets.
   ord_subtract(Set1,Intersection,MinimalSet1),
   ord_subtract(Set2,Intersection,MinimalSet2),
   distribute(
      MinimalSet1,MinimalSet2,
      Intersection,
      ReducedSet1,ReducedSet2
   ),
   assertion(ord_disjoint(ReducedSet1,ReducedSet2)), % re-check what we got
   assertion(ReducedSet1 \= []),                     % re-check what we got
   assertion(ReducedSet2 \= []).                     % re-check what we got

% ---
%% domain_propagation_for_male(+Set1,+Set2,-ReducedSet1,-ReducedSet2)
%
% Make the two bagua sets (which are ordered sets implemented as lists)
% "compatible" according to the book of changes!
%
% This is a fully deterministic operation if we keep to removing the least number of
% baguas from either set. 
% ---

domain_propgation_for_male(Set1,Set2,ReducedSet1,ReducedSet2) :-
   reduce_till_settled(
      clean_left,
      Set1,Set2,
      ReducedSet1,ReducedSet2
   ).

% Helpers for the above
%
% We can conceptualize this as a state machine that alternatingly removes baguas
% from the "left" (the baguas associated to the major constellation) and the "right"
% (the baguas associated to the minor constellation). The state change diagram is:
%
% clean_left -> clean_right --(no changes)--> done
%                   ^   |
%                   |   +-----(changes)--> clean_left_again -->(no changes)--> done
%                   |                          |
%                   +-----(changes)------------+

next_reduction_state(Set,NextSet,clean_right,clean_left_again) :- Set \= NextSet.
next_reduction_state(Set,NextSet,clean_right,done)             :- Set = NextSet.
next_reduction_state(Set,NextSet,clean_left_again,clean_right) :- Set \= NextSet.
next_reduction_state(Set,NextSet,clean_left_again,done)        :- Set = NextSet.
next_reduction_state(_,_,clean_left,clean_right).

reduce_till_settled(done,Set1,Set2,Set1,Set2).
 
reduce_till_settled(State,Set1,Set2,ReducedSet1,ReducedSet2) :-
   member(State,[clean_left,clean_left_again]),
   findall(
      M,
      (member(M,Set1),book_of_changes(M,N),member(N,Set2)),
      NextSet1X
   ),
   list_to_ord_set(NextSet1X,NextSet1), % this should not be needed
   ((NextSet1 == [],verbose) -> format("Domain propagation reduced the domain to empty: ~w -> []~n",[Set1]); true),
   NextSet1 \== [], % fails if empty, we do not accept "no baguas left"
   next_reduction_state(Set1,NextSet1,State,NextState),
   reduce_till_settled(NextState,NextSet1,Set2,ReducedSet1,ReducedSet2).

reduce_till_settled(clean_right,Set1,Set2,ReducedSet1,ReducedSet2) :-
   findall(
      N,
      (member(N,Set2),book_of_changes(M,N),member(M,Set1)), % this was unclear in the spec
      NextSet2X
   ),
   list_to_ord_set(NextSet2X,NextSet2), % this should not be needed
   ((NextSet2 == [],verbose) -> format("Domain propagation reduced the domain to empty: ~w -> []~n",[Set2]); true),
   NextSet2 \== [], % fails if empty, we do not accept "no baguas left"
   next_reduction_state(Set2,NextSet2,clean_right,NextState),
   reduce_till_settled(NextState,Set1,NextSet2,ReducedSet1,ReducedSet2).

% ---
%% build_list_of_constellations_affected(+C1,+C2,+BaguasOfC1Changed,+BaguasOfC2Changed,-List)
%
% Collect the constellation names into a list.
% ---

build_list_of_constellations_affected(C1,C2,true,true,[C1,C2]).
build_list_of_constellations_affected(C1,_,true,false,[C1]).
build_list_of_constellations_affected(_,C2,false,true,[C2]).
build_list_of_constellations_affected(_,_,false,false,[]).

% ---
%% handle_changed_baguas(
%%    +C1,+C2,                   constellations implicated (C1: major constellation, C2: minor constellation)
%%    +ConstelDict               the current "ConstelDict", to be modified to a new "ConstelDictNext"
%%    +SMels,                    currently SAFE male children
%%    +BaguaSet1,                the baguas initially associated to C1, an ordered set
%%    +BaguaSet2,                the baguas initially associated to C2, an ordered set
%%    +BaguaReducedSet1,         the baguas reduced to make the child SAFE, for C1, an ordered set
%%    +BaguaReducedSet2,         the baguas reduced to make the child SAFE, for C2, an ordered set
%%    -NewlyUnsafeMels,          the male children considered UNSAFE under "ConstelDictNext"
%%    -StillSafeMels,            the male children still considered SAFE under "ConstelDictNext"
%%    -ConstelDictNext           the next mapping "constellation -> bagua set"
%% )
%
% Helper for search/5 to create the next "ConstelDict" and 
% collect the males rendered UNSAFE under this dict.
% ---

handle_changed_baguas(C1,C2,ConstelDict,SMels,BaguaSet1,BaguaSet2,BaguaReducedSet1,BaguaReducedSet2,NewlyUnsafeMels,StillSafeMels,ConstelDictNext) :-
   reify(BaguaSet1\=BaguaReducedSet1,C1Changed),
   reify(BaguaSet2\=BaguaReducedSet2,C2Changed),
   ((C1Changed == true,verbose) -> format("   Baguas for '~w' changed: ~w -> ~w~n",[C1,BaguaSet1,BaguaReducedSet1]) ; true),
   ((C2Changed == true,verbose) -> format("   Baguas for '~w' changed: ~w -> ~w~n",[C2,BaguaSet2,BaguaReducedSet2]) ; true),
   put_dict(C1,ConstelDict,BaguaReducedSet1,ConstelDictX),
   put_dict(C2,ConstelDictX,BaguaReducedSet2,ConstelDictNext),
   build_list_of_constellations_affected(C1,C2,C1Changed,C2Changed,CList),
   collect_males_made_unsafe(CList,SMels,NewlyUnsafeMels,StillSafeMels).

% ---
%% search(+ConstelDict,+UMels,+UFems,+SMels,-ConstelDictOut)
%
% The recursive search for a solution.
% ConstelDict    - Current mapping: "constellation -> set-of-bagua", input
% UMels          - Potentially UNSAFE male children, input (equal to all the male children on first call of recursion)
% UFems          - Potentially UNSAFE female children, input (equal to all the female children on first call of recursion)
% SMels          - Tentatively SAFE male children, input (empty on first call of recursion)
%                  The tentatively SAFE female children are definitely SAFE and are not tracked.
% ConstelDictOut - Final mapping: "constellation -> set-of-bagua", the solution, output
% ---

% Examine top of the UMels (potentially UNSAFE males) list.
% If there is a male child that is SAFE under the current ConstelDict:
% Move it to the front of SMels (front because that's computationally cheap),
% the "tentatively SAFE male children" list.

search(ConstelDict,[child(C1,C2,male)|UMels],UFems,SMels,ConstelDictOut) :-
   is_safe(C1,C2,male,ConstelDict), 
   (verbose -> format("Found currently safe male child (~w,~w)~n",[C1,C2]) ; true),
   % create a new list of "currently safe" male children
   SMelsNext = [child(C1,C2,male)|SMels],
   % Recursive call follows:
   in_search(ConstelDict,UMels,UFems,SMelsNext,ConstelDictOut).

% Examine top of the UMels (potentially UNSAFE males) list.
% If there is a male child that is UNSAFE under the current ConstelDict:
% Proceed with deterministic removal of baguas attached to the male child's constellations
% (i.e. perform domain propagation on the domains of two constellations).

search(ConstelDict,[child(C1,C2,male)|UMels],UFems,SMels,ConstelDictOut) :-
   \+ is_safe(C1,C2,male,ConstelDict),
   (verbose -> format("Found currently unsafe male child (~w,~w)~n",[C1,C2]) ; true),
   get_dict(C1,ConstelDict,BaguaSet1),
   get_dict(C2,ConstelDict,BaguaSet2),
   (verbose -> format("   '~w' baguas: ~w~n",[C1,BaguaSet1]); true),
   (verbose -> format("   '~w' baguas: ~w~n",[C2,BaguaSet2]); true),
   assertion(C1\=C2), % Domain propagation was written assuming the constellations differ C1\=C2
   % We now perform DETERMINISTIC reduction of the bagua sets!
   domain_propgation_for_male(BaguaSet1,BaguaSet2,BaguaReducedSet1,BaguaReducedSet2),
   handle_changed_baguas(
      C1,C2,ConstelDict,SMels,BaguaSet1,BaguaSet2,BaguaReducedSet1,BaguaReducedSet2,
      NewlyUnsafeMels,StillSafeMels,ConstelDictNext
   ),
   % Child must now be SAFE under ConstelDictNext.
   assertion(is_safe(C1,C2,male,ConstelDictNext)),
   % Create a new list of UNSAFE male children by appending the male children newly found
   % to be UNSAFE under ConstelDictNext to the previously considered UNSAFE male children
   % except the one just processed (Umels).
   append([UMels,NewlyUnsafeMels],UMelsNext),
   % Create a new list of tentatively SAFE male children, by prepending the one just processed.
   SMelsNext = [child(C1,C2,male)|StillSafeMels],
   length(UMels,L0),
   length(SMels,L1),
   length(UMelsNext,L2),
   length(SMelsNext,L3),
   assertion(1 + L0 + L1 =:= L2 + L3),
   % Recursive call follows:
   in_search(ConstelDictNext,UMelsNext,UFems,SMelsNext,ConstelDictOut).

% Examine top of the UFems (potentially UNSAFE females) list, with the
% condition that the list of UMels (potentially UNSAFE males) is empty.
% If there is a female child that is SAFE under the current ConstelDict:
% A female SAFE child will never be made UNSAFE by our only domain propgation
% operation "bagua removal", we can label that child as definitely SAFE, 
% which means "forget about her".

search(ConstelDict,[],[child(C1,C2,female)|UFems],SMels,ConstelDictOut) :-
   is_safe(C1,C2,female,ConstelDict),
   (verbose -> format("Found safe female child (~w,~w)~n",[C1,C2]); true),
   % Recursive call follows, dropping the SAFE female child:
   in_search(ConstelDict,[],UFems,SMels,ConstelDictOut).

% Examine top of the UFems (potentially UNSAFE females) list, with the
% condition that the list of UMels (potentially UNSAFE males) is empty.
% If there is a female child that is UNSAFE under the current ConstelDict:
% Proceed with nondeterministic removal of baguas attached to the female child's 
% constellations, rendering the two sets of baguas disjoint.
% (i.e. perform domain propagation on the domains of two constellations).
% Once rendered SAFE the female child will never be made UNSAFE by our only
% domain propagation operation "bagua removal", so we can label that child as 
% definitely SAFE, which means "forget about her" (of course, backtracking over
% solutions later may unwind the stack of search/5 calls back to this point, and
% the child may reappear as something to work on).
 
search(ConstelDict,[],[child(C1,C2,female)|UFems],SMels,ConstelDictOut) :-
   \+ is_safe(C1,C2,female,ConstelDict),
   (verbose -> format("Found unsafe female child (~w,~w)~n",[C1,C2]); true),
   get_dict(C1,ConstelDict,BaguaSet1),
   get_dict(C2,ConstelDict,BaguaSet2),
   (verbose -> format("   '~w' baguas: ~w~n",[C1,BaguaSet1]); true),
   (verbose -> format("   '~w' baguas: ~w~n",[C2,BaguaSet2]); true),
   assertion(C1\=C2), % Domain propagation was written assuming the constellations differ C1\=C2
   % We now have to make a NONDETERMINISTIC CHOICE to make the bagua sets disjoint!
   domain_propagation_for_female(BaguaSet1,BaguaSet2,BaguaReducedSet1,BaguaReducedSet2),
   handle_changed_baguas(
      C1,C2,ConstelDict,SMels,BaguaSet1,BaguaSet2,BaguaReducedSet1,BaguaReducedSet2,
      NewlyUnsafeMels,StillSafeMels,ConstelDictNext
   ),
   % Child must now be SAFE under ConstelDictNext.
   assertion(is_safe(C1,C2,female,ConstelDictNext)),
   length(SMels,L1),
   length(NewlyUnsafeMels,L2),
   length(StillSafeMels,L3),
   assertion(L1 =:= L2 + L3),
   % Recursive call follows:
   in_search(ConstelDictNext,NewlyUnsafeMels,UFems,StillSafeMels,ConstelDictOut).

% If there is no male child that is UNSAFE and no female child that is UNSAFE, we are done!
% The passed-in ConstelDict is passed out again as ConstelDict.

search(ConstelDict,[],[],_,ConstelDict).

% ---
%% in_search(+ConstelDict,+UMels,+UFems,+SMels,-ConstelDictOut)
% An "interstitial call" for search/5.
% Called by search/5, prints out the current state, calls search/5 as
% as search/5 recurses looking for a solution.
% ---

in_search(ConstelDict,UMels,UFems,SMels,ConstelDictOut) :-
   length(UMels,L0),
   length(UFems,L1),
   length(SMels,L2),
   format("There are currently ~d unsafe males, ~d unsafe females, ~d tentatively safe males~n",[L0,L1,L2]),
   search(ConstelDict,UMels,UFems,SMels,ConstelDictOut).
 
% ---
%% partition_by_sex(+Children,-Mels,-Fems)
% ---

partition_by_sex(Children,Mels,Fems) :-
   partition(
      [Child]>>(Child=child(_,_,male)),
      Children,
      Mels,Fems).

% ---
%% search(+ConstelDict,+Children,-ConstelDictOut)
% Start the search for a solution.
% ---

search(ConstelDict,Children,ConstelDictOut) :-
   % separate children by sex for easier handling
   partition_by_sex(Children,UMels,UFems),
   in_search(ConstelDict,UMels,UFems,[],ConstelDictOut).
 
% ---
%% children_are_safe(+ConstelDict,+Children) :- 
% Apply the predicate is_safe/4 to all children (i.e. test whether all the chldren pass is_safe/4)
% ---

all_children_are_safe(ConstelDict,Children) :- 
   forall(
      member(child(C1,C2,Sex),Children),
      is_safe(C1,C2,Sex,ConstelDict)).

% ---
% Call this on the command line:
%
% load:
% ?- ["cursed_village_problem.pl"].
%
% run for 1 solution, hit ";" for more:
% ?- main(Sol)
%
% collect solutions (the stack will overflow eventually)
% ?- bagof(Sol,main(Sol),Bag).
% ---

verbose :- false. % set to true for more printing

main(ConstelDictOut) :- 
   verify_database,
   format("Verification of database succeeded~n"),   
   load_constellation_to_bagua_set_mapping_into_dict(ConstelDict),
   load_children_into_list(Children),
   !, % not gonna backtrack into the loading above
   % >>>
   search(ConstelDict,Children,ConstelDictOut), % The user may backtrack into search/3
   % <<<
   % On success, verify that children in the original list of children are indeed
   % SAFE under ConstelDictOut, which is "a" sought "constellation -> bagua set" mapping.
   assertion(all_children_are_safe(ConstelDictOut,Children)),
   printout(ConstelDictOut).


