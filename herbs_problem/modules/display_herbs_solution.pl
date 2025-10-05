% ---
% Helper code to print results (i.e. the list of seeds).
% This file is included by the other Prolog files.
% ---

:- module(display_herbs_solution,[display/2]).

% Position 100 means we are done, expectin an empty seed list.

display([],100).

% Otherwise, for lesser positions, there is still something to print.

display([S|Seeds],Pos) :-
   Pos < 100,
   divmod(Pos,10,_,Col),
   format("~|~32t~a~32t~10+",[S]),
   ((Col == 9) -> format("~n",[]) ; format(",",[])),
   succ(Pos,PosNext),
   display(Seeds,PosNext).

