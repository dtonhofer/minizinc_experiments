include "globals.mzn";

int: N = 8;

set of int: ROWS = 1..N;
set of int: COLS = 1..N;

% ---
% FIRST REPRESENTATION
% The idea is to have a NxN board, where each board cell is
% labeled with sequence number of the knight's move index 1..NxN
% ---

set of int: MOVE_INDEX = 1..N*N;
set of int: MOVE_INDEX_LESS_ONE = 1..((N*N)-1);

array[ROWS,COLS] of var MOVE_INDEX: board;

% ---
% Start the tour at [1,1].
% We do not set a constraint on the end of the tour.
% ---

constraint board[1,1] = 1;

% ---
% Random additional information, hopefully breaking symmetry.
% The first constraint alone reduces search time 1m6s -> 22sec
% ---

constraint board[2,1] > board[1,2];
%constraint board[2,2] > board[3,3];
%constraint board[3,3] < board[4,4];

% ---
% SECOND REPRESENTATION
% listing the (row,col) by the move index
% ---

array[MOVE_INDEX] of var ROWS: knight_rows;
array[MOVE_INDEX] of var COLS: knight_cols;

% ---
% Link the first and second representations.
% Two ways of doing it!
%
% > With only the first constraint: 3s to solution
% > With only the second constraint: 19s to solution
% > With both constraints: 19s to solution
% ---

bool: use_constraint1 = true;
bool: use_constraint2 = false;

constraint use_constraint1 -> forall(m in MOVE_INDEX) (
   let {
      var ROWS: row = knight_rows[m];
      var COLS: col = knight_cols[m];
   } in   
      board[row,col] = m
);

constraint use_constraint2 -> forall(row in ROWS, col in COLS) (
   let { 
      var MOVE_INDEX: m = board[row,col];
   } in
      knight_rows[m] = row /\ knight_cols[m] = col
);

% ---
% Additionally, to direct search, specify the 8 possible knight moves.
%
%  +---+---+---+---+---+---> col
%  |   | 2 |   | 3 |   |
%  +---+---+---+---+---+
%  | 1 |   |   |   | 4 |
%  +---+---+---+---+---+
%  |   |   | X |   |   |
%  +---+---+---+---+---+
%  | 8 |   |   |   | 5 |
%  +---+---+---+---+---+
%  |   | 7 |   | 6 |   |
%  +---+---+---+---+---+
%  |
%  V
% row
%
% ---

set of int: KNIGHT_MOVE_ID = 1..8;

% ---
% Only allow knight's moves across knight_rows/knight_cols
% (this constraint is trivial to write now)
% ---

constraint forall(m in MOVE_INDEX_LESS_ONE) (
   let { 
      var int: abs_delta_row = abs(knight_rows[m+1]-knight_rows[m]);
      var int: abs_delta_col = abs(knight_cols[m+1]-knight_cols[m]);
   } in
      (abs_delta_row = 1 /\ abs_delta_col = 2)
      \/
      (abs_delta_row = 2 /\ abs_delta_col = 1)
);

% ---
% THIRD REPRESENTATION
% Hold an array of those moves.
% This allows us to "search along the sequence of moves"
% ---

array[MOVE_INDEX_LESS_ONE] of var KNIGHT_MOVE_ID: knight_move_ids;

% ---
% Link the array of "knights move ids" to the "knights move row/col" deltas
% ---

constraint forall(m in MOVE_INDEX_LESS_ONE) (
   let { 
      var int: delta_row = knight_rows[m+1]-knight_rows[m];
      var int: delta_col = knight_cols[m+1]-knight_cols[m];
   } in
      (knight_move_ids[m] = 1 <-> (delta_row = -1 /\ delta_col = -2)) /\
      (knight_move_ids[m] = 2 <-> (delta_row = -2 /\ delta_col = -1)) /\
      (knight_move_ids[m] = 3 <-> (delta_row = -2 /\ delta_col = +1)) /\
      (knight_move_ids[m] = 4 <-> (delta_row = -1 /\ delta_col = +2)) /\
      (knight_move_ids[m] = 5 <-> (delta_row = +1 /\ delta_col = +2)) /\
      (knight_move_ids[m] = 6 <-> (delta_row = +2 /\ delta_col = +1)) /\
      (knight_move_ids[m] = 7 <-> (delta_row = +2 /\ delta_col = -1)) /\
      (knight_move_ids[m] = 8 <-> (delta_row = +1 /\ delta_col = -2))            
);

% ---
% Every board square must be visited exactly once
% ---

constraint all_different([board[r, c] | r in ROWS, c in COLS]);

% ---
% Solving
% https://docs.minizinc.dev/en/stable/mzn_search.html
% ---

solve :: int_search(knight_move_ids, input_order, indomain_median) satisfy;

% ---
% Printing
% ---

output [
    concat( [ show_int(2, board[r, c] ) ++ " " | c in COLS] ) ++ "\n" | r in ROWS
];
output [
    concat( [ show_int(2, knight_move_ids[m] ) ++ " " | m in MOVE_INDEX_LESS_ONE] )
];

% With Chuffed 0.13.2
% on a "12th Gen Intel(R) Core(TM) i3-12100", 1 core being used
% using "int_search(knight_move_ids, input_order, indomain_median) satisfy;"
% a solution is found within seconds
% (Geocode 6.3.0 finds no solution even after 15 minutes)
% with:
%
% constraint board[2,1] > board[1,2];
%
% bool: use_constraint1 = false;
% bool: use_constraint2 = true; --> 20s
%
% bool: use_constraint1 = true;
% bool: use_constraint2 = false; --> 2s
%
% bool: use_constraint1 = true;
% bool: use_constraint2 = true; --> 22s
%
%
%  1 44 57 46  3 30 15 12
% 56 47  2 31 16 13 18  5
% 43 58 45 50 29  4 11 14
% 48 55 42 59 32 17  6 19
% 41 60 49 36 51 28 23 10
% 54 63 38 33 26  9 20  7
% 61 40 35 52 37 22 27 24
% 64 53 62 39 34 25  8 21
%
% 5  4  6  4  7  6  7  2  4  2  3  8  5  2  8  6  3  6  7
% 6  1  3  6  8  2  5  2  2  3  8  6  7  6  1  3  6  1  6
% 1  2  4  1  3  6  3  8  7  5  3  6  7  8  2  3  2  4  7
% 5  8  7  5  2  7
% ----------
% Finished in 2s 996msec.
%

