#!/usr/bin/perl

######
# This script runs MiniZinc repeatedly on the same model with different data
# files and different search annotations (to apply different variable selection
# strategies and domain splitting strategies), and collects information about
# the run in a CSV file
#
# The homepage for this script with additional explanation is at
# https://github.com/dtonhofer/minizinc_experiments/tree/main/stats_collection
#
# ----
# See the MiniZinc manual for annotations and search at:
#
# https://www.minizinc.org/doc-2.5.5/en/mzn_search.html
#
# The complete list of currently available search annotations is at
#
# https://www.minizinc.org/doc-2.5.5/en/fzn-spec.html?highlight=dom_w_deg#search-annotations
#####

use warnings;
use strict;
use utf8;     # Meaning "This lexical scope (i.e. file) contains utf8"

# The following causes non-ASCII chars to be printed in UTF-8. This is what should
# happen if the terminal expects UTF-8 encoding.
# https://perldoc.perl.org/perlunifaq.html

binmode STDERR, ':encoding(UTF-8)';
binmode STDOUT, ':encoding(UTF-8)';

# Various imports. You may have to install appropriate packages.

use File::Basename;
use File::Spec;
use Getopt::Long;
use POSIX; # for getcwd

# Extend the library path with a local directory holding our Perl Modules.
#
#                      |
#            +---------+---------+
#            |                   |
#     this script = $0        modules/
#                                |
#                               Mzn/
#                                |
#                            ReadYaml.pm
#                     (module mzn::read_yaml)
#
# See https://perlmaven.com/how-to-create-a-perl-module-for-code-reuse

use lib File::Spec->catfile(dirname($0),'modules');

use Mzn::ReadYaml
   qw(read_and_interprete_yaml);

use Mzn::ResultFiling
   qw(touch_resultfile
      rename_resultfile_if_exists);

use Mzn::TaskQueue
   qw(build_task_queue
      scramble_task_queue
      create_logs_dir_if_missing);

use Mzn::Forking
   qw(main_loop);

# Whether the minizinc command line will be added to the log files.

my $write_cmdline = 1;

# "aliases_vss" : Known "Variable Selection Strategies", to be
#                 complemented with YAML file data.
#                 By default, the aliases just map to
#                 themselves.
# "aliases_dss" : Known "Domain Splitting Strategies", to be
#                 complemented with YAML file data.
#                 By default, the aliases just map to
#                 themselves.
# "config"      : Known configurations to be used for data files,
#                 to be filled with YAML file data.
# "datafiles"   : Known data files to process, to be filled with
#                 YAML file data.
# "modelfiles"  : Model files to process. Generally only 1.

my $aliases_vss = {
     input_order       => 'input_order'      # choose in order from the array
    ,first_fail        => 'first_fail'       # choose the variable with the smallest domain size
    ,anti_first_fail   => 'anti_first_fail'  # choose the variable with the largest domain
    ,smallest          => 'smallest'         # choose the variable with the smallest value in its domain
    ,largest           => 'largest'          # choose the variable with the largest value in its domain
    ,occurrence        => 'occurence'        # choose the variable with the largest number of attached constraints
    ,most_constrained  => 'most_constrained' # choose the variable with the smallest domain, breaking ties using the number of constraints
    ,max_regret        => 'max_regret'       # choose the variable with the largest difference between the two smallest values in its domain.
    ,dom_w_deg         => 'dom_w_deg'        # choose the variable with the smallest value of domain size divided by weighted degree, which is the number of times it has been in a constraint that caused failure earlier in the search
};

my $aliases_dss = {
     indomain_min           => 'indomain_min'           # assign the smallest value in the variable's domain
    ,indomain_max           => 'indomain_max'           # assign the largest value in the variable's domain
    ,indomain_middle        => 'indomain_middle'        # assign the value in the variable’s domain closest to the mean of its current bounds
    ,indomain_median        => 'indomain_median'        # assign the middle value in the variable’s domain
    ,indomain               => 'indomain'               # nondeterministically assign values to the variable in ascending order
    ,indomain_random        => 'indomain_random'        # assign a random value from the variable’s domain
    ,indomain_split         => 'indomain_split'         # bisect the variable’s domain, excluding the upper half first.
    ,indomain_reverse_split => 'indomain_reverse_split' # bisect the variable’s domain, excluding the lower half first.
    ,indomain_interval      => 'indomain_interval'      # if the variable’s domain consists of several contiguous intervals, reduce the domain to the first interval. Otherwise just split the variable’s domain.
};

my $configs    = {};
my $datafiles  = {};
my $modelfiles = [];

# Get information from the command line. process_cmdline_options() exits on error.

my ( $fq_configfile
    ,$debug_config
    ,$debug_tasks
    ,$debug_results
    ,$work_dir
    ,$scramble
    ,$parallel_procs
    ,$keep_logs,
    ,$dry_run ) = process_cmdline_options();

# The $work_dir has been given on the command line. Underneath, we expect this:
#
#                                     $work_dir/
#                                         |
#               +-----------------+-------+--------+-----------------+
#               |                 |                |                 |
#             data/           models/           logs/           result.txt
#          $fq_data_dir     $fq_model_dir   $fq_logs_dir      $fq_result_file
#               |                 |                |            (CSV file)
#              / \               / \              / \
#         (contains the    $fq_model_file     accumulates
#          @$datafiles)       (mzn file)    subprocess logs
#                                           which are removed
#                                            if the process
#                                           terminated with 0
#
# The existence of $work_dir has been checked during processing of the command line.

my $fq_model_dir  = "$work_dir/models";       # must exist (checked later)
my $fq_data_dir   = "$work_dir/data";         # must exist (checked later)
my $fq_logs_dir   = "$work_dir/logs";         # created if missing
my $fq_resultfile = "$work_dir/result.txt";   # CSV file; if exists, the old one is renamed

if (! -d $fq_data_dir) {
   print STDERR "Data directory '$fq_data_dir' does not exist -- exiting!\n";
   exit 1
}

if (! -d $fq_model_dir) {
   print STDERR "Model directory '$fq_model_dir' does not exist -- exiting!\n";
   exit 1
}

my $args = {
   configfile    => $fq_configfile,   # where the configfile is
   data_dir      => $fq_data_dir,     # where the data files are
   model_dir     => $fq_model_dir,    # where the model files are
   resultfile    => $fq_resultfile,   # where the result goes
   debug_config  => $debug_config,    # do debug printing
   aliases_vss   => $aliases_vss,     # to be filled via "read_and_interprete_yaml"
   aliases_dss   => $aliases_dss,     # as above
   configs       => $configs,         # as above
   datafiles     => $datafiles,       # as above
   modelfiles    => $modelfiles };    # as above

print STDERR "Configuring using config file '$fq_configfile'\n";

read_and_interprete_yaml($args);

if (@$modelfiles == 0) {
   print STDERR "After examining the configuration, no model files remain -- exiting!\n";
}

# build hash describing minizinc tasks to be run

my $task_queue = build_task_queue($args);

if ($debug_tasks || $dry_run) {
   print STDERR Data::Dumper->new([$task_queue])->Sortkeys(1)->Dump
}

if ($dry_run) {
   print STDERR "Just a dry run -- exiting!\n";
   exit 0
}

if ($scramble) {
   $task_queue = scramble_task_queue($task_queue)
}

create_logs_dir_if_missing($fq_logs_dir);
rename_resultfile_if_exists($fq_resultfile);
touch_resultfile($fq_resultfile);

my $add_cmdline_to_logs = 1;

main_loop($task_queue,$parallel_procs,$fq_logs_dir,$add_cmdline_to_logs,$keep_logs,$debug_results);

exit 0;

# -----------

sub process_cmdline_options {
   my $help;
   my $configfile;
   my $debug_config;
   my $work_dir;
   my $scramble;
   my $parallel_procs = 1;
   my $keep_logs;
   my $debug_tasks;
   my $debug_results;
   my $dry_run;
   # https://perldoc.perl.org/Getopt::Long
   GetOptions(
       "help"         => \$help
      ,"debugcfg"     => \$debug_config
      ,"debugtasks"   => \$debug_tasks
      ,"debugresults" => \$debug_results
      ,"dryrun"       => \$dry_run
      ,"scramble"     => \$scramble
      ,"keeplogs"     => \$keep_logs
      ,"cfg=s"        => \$configfile
      ,"parallel=i"   => \$parallel_procs
      ,"workdir=s"    => \$work_dir)
     or die "Error in command line arguments!"; # Generally if there are unknown args
   my $error_count = 0;
   if (!$configfile && !$help) {
      print STDERR "Configuration file must be specified.\n";
      $error_count++
   }
   if (!$work_dir && !$help) {
      $work_dir = getcwd();
      print STDERR "Note: Work dir not specified, using current directory '$work_dir'.\n";
   }
   if ($parallel_procs <= 0 || $parallel_procs > 16) {
      print STDERR "Value of 'parallel' should be from 1 to 16.\n";
      $error_count++
   }
   if ($help || $error_count>0) {
      my $msg = << "EOF";
--cfg=<YAML configuration file> : set configuration file.
--workdir=<toplevel work dir>   : set the directory under which models and
                                  data files reside. If missing, the current
                                  directory is used.
--parallel=<number>             : start 'number' parallel MiniZinc processes
                                  to work off all the tasks. Default is 1.
--debugcfg                      : print debug info after configuration process.
--debugtasks                    : print the tasks (args for MiniZinc executions).
--debugresults                  : print the results obtained from MiniZinc.
--scramble                      : scramble the task queue for more randomness.
--keeplogs                      : do not delete MiniZinc logs after completion.
--dryrun                        : Just print the tasks that would be run.
EOF
      print STDERR $msg;
      if ($error_count>0) {
         exit 1
      }
      else {
         exit 0
      }
   }
   if (! -d $work_dir) {
      print STDERR "The work directory '$work_dir' in which we shall work does not exist or is not a directory -- exiting\n";
      exit 1
   }
   my $fq_configfile = File::Spec->catfile($work_dir,$configfile);
   if (! -f $fq_configfile) {
      print STDERR "The config file '$fq_configfile' does not exist or is not a file -- exiting\n";
      exit 1
   }
   return ($fq_configfile,$debug_config,$debug_tasks,$debug_results,$work_dir,$scramble,$parallel_procs,$keep_logs,$dry_run)
}


