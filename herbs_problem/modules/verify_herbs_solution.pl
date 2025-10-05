% ---
% Helper code to verify a fully instantiated list of seed to see
% whether it indeed fulfills all the constraints.
% This file is included by the other Prolog files.
% ---

:- module(verify_herbs_solution,[verify/1]).

:- use_module('extract_row_or_col.pl',[extract_col/3,extract_row/3]).

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

verify_col(Col,ChosenElement,Seeds) :-
   extract_col(Col,Seeds,ColEntries),
   % https://www.swi-prolog.org/pldoc/doc_for?object=aggregate/3
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

verify_with_complaint(Goal,Text,Args) :-
   Goal -> true ; (format(Text,Args),false).

verify(Seeds) :-
   verify_with_complaint((\+ nth0(0,Seeds,metal)),"First seed is actually 'metal'~n",[]),
   numlist(0, 9, Indexes),
   forall(member(Row,Indexes),
      verify_with_complaint(verify_row(Row,Seeds),"Row ~d is faulty~n",[Row])
   ),
   forall(member(Col,Indexes),
      verify_with_complaint(verify_col(Col,metal,Seeds),"Col ~d is bad for 'metal'~n",[Col])
   ),
   forall(member(Col,Indexes),
      verify_with_complaint(verify_col(Col,earth,Seeds),"Col ~d is bad for 'earth'~n",[Col])
   ).

