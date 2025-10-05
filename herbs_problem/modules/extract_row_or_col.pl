% ---
% Helper code to extract a row or col from the list of seeds.
% This file is included by the other Prolog files.
% ---

:- module(extract_row_or_col,[extract_col/3,extract_row/3]).

% We will be using the CLP(FD) predicate "#=" for integer equality
% rather than the less declarative "is/2" and "=:=/2"

:- use_module(library(clpfd)).

% ---
% Extracting a column or a row from a linear list
% ---

extract_col(Col,Seeds,ColEntries) :-
   assertion(between(0,9,Col)),
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
   assertion(between(0,9,Row)),
   bagof(Element,
         Col^Index^(
            between(0,9,Col),
            Index #= Col+Row*10,
            nth0(Index,Seeds,Element)
         ),
         RowEntries),
   % format("Extracted row ~d is ~w~n",[Row,RowEntries]),
   true. % only exists to make the previous line out-commentable

