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
% SWI Prolog's CLP(FD), constraint logic programming over finite domains,
% by Markus Triska.
%
% https://www.swi-prolog.org/pldoc/man?section=clpfd
%
% This code runs in SWI-Prolog 9.3.5.
% ---

:- use_module(library(clpfd)).
:- use_module('modules/extract_row_or_col.pl').
:- use_module('modules/verify_herbs_solution.pl').
:- use_module('modules/display_herbs_solution.pl').

% ---
% Mapping between elements and integers that designate them
% ---

element(metal, 1).
element(wood,  2).
element(water, 3).
element(fire,  4).
element(earth, 5).

% ---
% Contraint between two adjacent cells (aka. vars/drawers/cols) in each row.
% We are using an "automaton" which accepts if the constraint is upheld:
% https://www.swi-prolog.org/pldoc/doc_for?object=automaton/3
% as there seems to be no way to express a constraint using a
% predicate directly as with "table" in MiniZinc:
% https://docs.minizinc.dev/en/stable/lib-globals-extensional.html
%
% An alternative would be to create an "automaton" for
% a whole row instead of just two adjacent cells.
% ---

constraint_on_single_row(Row,Seeds,VarList) :-
   format("Setting up generative/destructive successor constraint on row ~d~n",[Row]),
   % retrieve numeric ids
   element(metal, Metal), 
   element(wood,  Wood),
   element(water, Water),
   element(fire,  Fire),
   element(earth, Earth),
   % build a list of entries of row "Row" (which is all unbound vars)
   extract_row(Row,Seeds,VarList),
   automaton(VarList,
             [source(a),sink(b),sink(c),sink(d),sink(e),sink(f)],
             [% starting the automaton
              arc(a,Water,b),
              arc(a,Wood,c),
              arc(a,Fire,d),
              arc(a,Earth,e),
              arc(a,Metal,f),
              % "generative" relationships
              arc(b,Wood,c), 
              arc(c,Fire,d),
              arc(d,Earth,e),
              arc(e,Metal,f),
              arc(f,Water,b),
              % "destructive" relationships 
              arc(b,Fire,d),
              arc(d,Metal,f),
              arc(f,Wood,c),
              arc(c,Earth,e),
              arc(e,Water,b)]).
 
% ---
% "Every row has a generative/destructive constraints (on successive vars)"
% Note that we need to RETAIN the pairs of vars on which those constraints
% are set up! We put them into the list "AllListOfPairs", which is then 
% communicated to the caller. If we just set up the constraints and discard the 
% lists, the constraints are not live!
% ---

constraints_on_all_rows(Seeds,AllVarLists) :-
   bagof(VarList,
         (
            between(0,9,Row),
            constraint_on_single_row(Row,Seeds,VarList)
         ),
         AllVarLists).

% ---
% Column constraint: "Both metal and earth appear once or twice."
% The list of vars on which the contraint has been set up is communicated
% to the caller in "ColVars", for retention.
% ---

constraint_on_single_column(Col,Seeds,VarList) :-
   format("Setting up metal/earth counting constraint on column ~d~n",[Col]),
   % retrieve numeric ids
   element(metal, Metal),
   element(wood,  Wood),
   element(water, Water),
   element(fire,  Fire),
   element(earth, Earth),
   % build a list of entries of column "Col" (which is all unbound vars)
   extract_col(Col,Seeds,VarList),
   % set global constraint over those vars
   % https://www.swi-prolog.org/pldoc/doc_for?object=global_cardinality/2
   global_cardinality(VarList, [Metal-S1,Earth-S2,Water-_,Fire-_,Wood-_]),
   S1 in 1..2,
   S2 in 1..2.

% ---
% "Every column has a counting constraint (over the column)"
% Note that we need to RETAIN the lists of vars on which those constraints
% are set up! We put them into the list "AllVarLists", which is then communicated
% to the caller. If we just set up the constraints and discard the lists,
% the constraints are not live!
%

constraints_on_all_columns(Seeds,AllVarLists) :-
   bagof(VarList,
         (
            between(0,9,Col),
            constraint_on_single_column(Col,Seeds,VarList)
         ),
         AllVarLists).

% ---
% "First see cannot be metal"
% ---

constraint_on_first_seed(Seeds) :-
   [FirstSeed|_] = Seeds,
   element(metal, Metal), % get the integer for Metal 
   FirstSeed #\= Metal. % first seed cannot be metal

% ---
% Translating from integers to atoms
% ---

translate([],[]).
translate([Seed|Seeds],[Atom|Atoms]) :-
   element(Atom,Seed),
   translate(Seeds,Atoms).

% ---
% Kick off the search, then print the results.
% ?- ['herbs_problem_using_clpfd.pl'].
% ?- main.
% ---

main :-

    % We are looking for 100 seeds (represented by their associated elements).
    % Set up a list of 100 vars. The list is conceptually divided into rows and 
    % columns by considering as a flattened 10x10 matrix which "varies by column"
    % fastest, "varies by row" slowest.
 
    length(Seeds, 100),    

    % Constraint: all the vars in AllVars range over 1..5
    % https://www.swi-prolog.org/pldoc/doc_for?object=ins/2

    Seeds ins 1..5,

    % Further constraints. We need to "retain" the lists of 
    % vars over which constraints exist or else those constraints
    % will be forgotten about.

    constraint_on_first_seed(Seeds),
    constraints_on_all_columns(Seeds,_AllVarListsCols),
    constraints_on_all_rows(Seeds,_AllVarListsRows),
 
    % Labeling, i.e. "find a concrete solution" (not quite the same as
    % the "labeling" of MiniZinc, this is the "solve" of MiniZinc.)
    % The first argument is a list of options.
    % https://www.swi-prolog.org/pldoc/doc_for?object=labeling/2

    labeling([leftmost], Seeds),

    translate(Seeds,SeedsAsAtoms),

    display(SeedsAsAtoms,0),
    assertion(verify(SeedsAsAtoms)).


