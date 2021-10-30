% Modified model as used in Workshop 9 of the Coursera Course
% "Solving Algorithms for Discrete Optimization" at
% https://www.coursera.org/learn/solving-algorithms-discrete-optimization

% ---
% Parameters, configured with data files
% ---

int: n;                             % parameter: number of tasks
set of int: TASK = 1..n;            % TASK is used as a range, ordered set, or domain from which to pick elements
array[TASK] of int:  d;             % parameter: array of task duration

int: m;                             % parameter: number of binary "precedence" relationships
set of int: PREC = 1..m;            % PREC is used as a range, ordered set, or domain from which to pick elements
array[PREC] of TASK:  pre;          % parameter: task pre[i] must finish
array[PREC] of TASK:  post;         % before post[i] starts

int: o;                             % parameter: number of "disjoint" sets (hypergraph edges, really)
set of int: DISJ = 1..o;            % DISJ is used as a range, ordered set, or domain from which to pick elements
array[DISJ] of set of TASK: disj;   % sets of tasks which cant overlap in time

% ---
% Variables describing a configuration we are looking for
% ---

int: maxt = sum(t in TASK)(d[t]);   % Lax bound on max time: the time quantum past the end of all tasks done seuqentially

array[TASK] of var 0..maxt: s;      % start time of a task; note that we may start at time 0
array[TASK] of var 0..maxt: e       % end time of a task; note that this is the index of time quantum *after* the task finishes
   = [ s[t] + d[t] | t in TASK];

% ---
% A configuration must fulfill the following to be a solution:
% ---

% Obey precedence relationships. Note the <= because "e[]" is already after the task proper

constraint forall(i in PREC) (e[pre[i]] <= s[post[i]]);

% b[,] is the scoreboard of the "comes before" relationship. Each point is either true or false.

% Restrict yourself arbitrarily to examining the "comes before" relationship between two tasks
% t1, t2 only if (numerically) t1 < t2, i.e. restrict yourself to the part above the diagonal
% in the b[,] scoreboard.

% We are not interested in the values on or below the diagonal, so force them these 0.
% Additionally, if two tasks t1,t2 with t1<t2 are not disjoint ("can run at the same time")
% set b[t1,t2] arbitrarily to 0.

array[TASK,TASK] of var 0..1: b;

constraint forall(t1, t2 in TASK
                  where
                  t1 >= t2
                  \/
                  not exists(i in DISJ)({t1,t2} subset disj[i]))
              (b[t1,t2] = 0);

% For all the tasks that need to be disjoint, the scorboard b[t1,t2] is is chosen to
% be 1 implying "t1 comes before t2" or 0 implying "t2 comes before t1". Setting these
% b[t1,t2] configures a specific ordering fulfilling disjointness.

constraint forall(i in DISJ, t1, t2 in disj[i] where t1 < t2)
                 ( (b[t1,t2]  = 1 -> e[t1] <= s[t2]) /\
                   (b[t1,t2]  = 0 -> e[t2] <= s[t1]) );

% ---
% What we want to optimize
% ---

% Maximum endtime of a configuration should be as small as possible.

var 0..maxt: makespan = max(e);

% Annotation parameter to suggest search strategy, set during MiniZinc invocation.
% Note that it makes sense to try various strategies for "s" and "b" and even maybe "makespan",
% isolated or in combination.
%
% Additionally, a restart annotation may be set to cause restart if a certain
% number of nodes have been visited. By default the value of the restart annotation
% is "restart_none" 

ann: my_search_annotation;
ann: my_restart_annotation;

solve :: my_search_annotation :: my_restart_annotation minimize makespan;

% ---
% Ouptut
% ---

output ["makespan = ", show(makespan), "\n" ] ++
       [ show_int(3,s[t]) ++ " "
       | t in TASK];

