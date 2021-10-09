% ------------------------------
% Output magic for "Assignment 10" in Week 2 of the Coursera Course
% "Solving Algorithms for Discrete Optimization" by Prof. Jimmy Ho Man Lee and Prof. Peter James Stuckey,
% which has its homepage at https://www.coursera.org/learn/solving-algorithms-discrete-optimization
% ------------------------------
 
function string: taskset_to_string(set of TASK: s) =
  if (card(s) = 0) then "" else join(",",[ show(task) | task in s ]) endif;
 
int: cook_max_strlen = max([ string_length(show(cook)) | cook in COOK ] ++ [string_length("t-XXX")]);
 
function string: taskset_to_output(set of TASK: s) =
   if (card(s) = 0) then ""
   elseif (card(s) = 1) then " comes before task " ++ taskset_to_string(s)
   else " comes before tasks " ++ taskset_to_string(s)
   endif;
 
% Personal note: I think MiniZinc needs an alternative programmer-friendly syntax
% rather than a syntax inspired by 20th century traditional mathematical notation.
% This is harder to read than LISP.
 
array[TASK] of set of TASK: later_tasks =
   [
      { later_task
      | later_task in TASK
        where (
           task < later_task
           /\
           exists( [ { task, later_task } subset steps[dish]
                     /\
                     not exists( [ ( { task, in_between_task, later_task } subset steps[dish]
                                     /\
                                     task < in_between_task /\ in_between_task < later_task )
                                 | in_between_task in TASK
                                 ] )
                   | dish in DISH
                   ] )
        ) % where
      }
      | task in TASK ];
 
output fix([ "% task " ++ show_int(3,task) ++ ":" ++
         " cook = " ++ format_justify_string(cook_max_strlen,show(c[task])) ++
         " [ " ++ show_int(3,s[task]) ++
         " ~ " ++ show_int(3,time[task,c[task]]) ++ " ~ " ++
         show_int(3,e[task]) ++ " ] " ++
         taskset_to_output(later_tasks[task])
         ++ "\n" | task in TASK ]);

output [ "\n" ];
 
% ---
% Also print in tabular form
% ---
 
% Trying to move printing out into functions won't work (but I'm not sure why):
% I get "MiniZinc: evaluation error: expression is not fixed". Tried to work around that
% but no dice, so eventually I inlined the code.
 
/*
function string: matching_task_to_string(COOK: cook, TASK: task, int: time) :: output_only =
   if
      fix(c[task]) = cook /\ fix(s[task]) <= time /\ time <= fix(e[task])
   then
      show(task)
   else "" endif;
 
function string: over_all_tasks(COOK: cook, int: time) =
   format_justify_string(cook_max_strlen, concat([matching_task_to_string(cook,task,time) | task in TASK]));
 
array[TIME] of string: table_row ::output_only = array1d(TIME,[ join("|", [ over_all_tasks(cook,time) | cook in COOK ]) | time in TIME ]);
*/
 
array[TIME] of string: table_row ::output_only = array1d(TIME,[ join("|", [ format_justify_string(cook_max_strlen, concat([if
      fix(c[task]) = cook /\ fix(s[task]) <= tx /\ tx <= fix(e[task])
   then
      "t-" ++ show(task)
   else "" endif | task in TASK])) | cook in COOK ]) | tx in TIME ]);
 
% Table header
output [ "%       " ++ "    " ++ "|" ++ join("|",[format_justify_string(cook_max_strlen,show(cook)) | cook in COOK ]) ++ "|\n"];
 
% All the table rows, one for each "time"
output [ concat( [
 
   if tx = 0 \/ tx = max(TIME) \/ (0<tx /\ tx<max(TIME) /\ (table_row[tx-1] != table_row[tx] \/ table_row[tx] != table_row[tx+1]))
   then
   "% time " ++ show_int(4,tx) ++ ":|" ++ table_row[tx] ++ "|"
   ++ ( if tx = fix(obj) then " ** OBJECTIVE \(obj) **\n" else "\n" endif )
   else ""
   endif
  | tx in TIME ])  ] ;

/* ----

This gives us output like the following, where the table has had all 
rows that are the same as a preceding and a subsequent row eliminated:

% maxt_of_course = 1224
% maxt           = 563

s = [0, 17, 47, 47, 97, 197, 163, 225, 305];
c = [BOB, CAROL, ALICE, TED, CAROL, ALICE, CAROL, TED, ALICE];
obj = 395;

% Task  1: cook =   BOB [   0 ~  17 ~  16 ]  comes before task 2
% Task  2: cook = CAROL [  17 ~  30 ~  46 ]  comes before tasks 3,4
% Task  3: cook = ALICE [  47 ~ 150 ~ 196 ] 
% Task  4: cook =   TED [  47 ~  50 ~  96 ]  comes before task 5
% Task  5: cook = CAROL [  97 ~  66 ~ 162 ]  comes before task 6
% Task  6: cook = ALICE [ 197 ~  78 ~ 274 ] 
% Task  7: cook = CAROL [ 163 ~  62 ~ 224 ]  comes before task 8
% Task  8: cook =   TED [ 225 ~  80 ~ 304 ]  comes before task 9
% Task  9: cook = ALICE [ 305 ~  90 ~ 394 ] 

%           |  BOB|CAROL|  TED|ALICE|
% time    0:|  t-1|     |     |     |
% time   16:|  t-1|     |     |     |
% time   17:|     |  t-2|     |     |
% time   46:|     |  t-2|     |     |
% time   47:|     |     |  t-4|  t-3|
% time   96:|     |     |  t-4|  t-3|
% time   97:|     |  t-5|     |  t-3|
% time  162:|     |  t-5|     |  t-3|
% time  163:|     |  t-7|     |  t-3|
% time  196:|     |  t-7|     |  t-3|
% time  197:|     |  t-7|     |  t-6|
% time  224:|     |  t-7|     |  t-6|
% time  225:|     |     |  t-8|  t-6|
% time  274:|     |     |  t-8|  t-6|
% time  275:|     |     |  t-8|     |
% time  304:|     |     |  t-8|     |
% time  305:|     |     |     |  t-9|
% time  394:|     |     |     |  t-9|
% time  395:|     |     |     |     | ** OBJECTIVE 395 **
% time  563:|     |     |     |     |

*/
