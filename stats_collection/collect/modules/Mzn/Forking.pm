#!/usr/bin/perl

###############################################################################
# Run MiniZinc repeatedly on the same modelfile with different datafiles and
# different search annotations (so as to apply different variable selection
# strategies and domain splitting strategies), and collect information about
# each run in a CSV file.
#
# The homepage for this script with additional explanation is at
# https://github.com/dtonhofer/minizinc_experiments/tree/main/stats_collection
###############################################################################

package Mzn::Forking;

use warnings;
use strict;
use utf8;  # Meaning "This lexical scope (i.e. file) contains utf8"

use Data::Dumper; # install with "dnf install perl-Data-Dumper"
use File::Spec;
use List::Util qw(max min);
use POSIX qw(strftime);

use Mzn::ResultFiling qw(write_to_resultfile);

use Exporter qw(import);
our @EXPORT_OK =
   qw(
      main_loop
     );

sub main_loop {
   my($task_queue,$parallel_procs,$logs_dir,$add_cmdline_to_logs,$keep_logs,$debug_results) = @_;
   my $children = {}; # Active child processes.
   while (@$task_queue > 0) {
      if (%$children >= $parallel_procs) {
         print STDERR "Waiting for an empty child slot...\n";
         wait_for_any_child_termination($children);
         die "There should be free slots now" unless %$children < $parallel_procs
      }
      my $task = shift @$task_queue;
      print STDERR "Submitting work. There are now " . scalar(@$task_queue) . " entries left in the task queue.\n";
      # >>>
      my $pid = fork_child_process($task,$children,$logs_dir,$add_cmdline_to_logs,$keep_logs,$debug_results);
      # <<<
      print STDERR "Started process $pid\n"
   }
   reap_all_children($children);
}

sub wait_for_any_child_termination {
   my($children) = @_;
   my $pid = wait(); # wait for any child
   if ($pid == -1) {
      print STDERR "No more child processes\n"
   }
   else {
      my $retval = $?;
      my $delay_s = time() - $$children{$pid};
      my $sss = ($delay_s == 1) ? "" : "s";
      # Not sure whether the lower 8 bit are set after wait (?)
      if ($retval & 127) {
         my $sigval = ($retval & 127);
         my $ww     = (($retval & 128) ? "with" : "without");
         printf STDERR "Child process with PID $pid died due to signal $sigval, $ww coredump, after $delay_s second$sss.\n";
      }
      else {
         # Good termination, see what the process said
         my $exit_status = ($retval >> 8);
         if ($exit_status > 0) {
            print STDERR "Child process with PID $pid terminated with non-zero exit status $exit_status, after $delay_s second$sss.\n";
         }
         else {
            print STDERR "Child process with PID $pid terminated with zero status after $delay_s second$sss.\n";
         }
      }
      die "There is no child with PID $pid in the children hash!" unless exists $$children{$pid};
      delete $$children{$pid}
   }
}

sub reap_all_children {
   my($children) = @_;
   while (%$children > 0) {
      wait_for_any_child_termination($children)
   }
}

sub fork_child_process {
   my($task,$children,$logs_dir,$add_cmdline_to_logs,$keep_logs,$debug_results) = @_;
   # >>>>>>>>
   my $pid = fork();
   # <<<<<<<<
   if (!defined($pid)) {
      die "Fork of child process was unsuccessful: $!"
   }
   elsif ($pid > 0) {
      # This is the parent process!
      die "Child pid $pid already exists in the children hash!" if exists $$children{$pid};
      $$children{$pid} = time();
      return $pid
   }
   else {
      # This is the child process - we can now start minizinc in here
      die unless ($pid == 0);
      # >>>>
      fork_minizinc($task,$logs_dir,$add_cmdline_to_logs,$keep_logs,$debug_results);
      # <<<<
      exit 0; # make sure you don't get back into the main loop in this subprocess
   }
}

sub fork_minizinc {
   my($task,$logs_dir,$add_cmdline_to_logs,$keep_logs,$debug_results) = @_;

   my $modelfile      = $$task{modelfile};   die "Model file '$modelfile' does not exist" unless -f $modelfile;
   my $datafile       = $$task{datafile};    die "Data file '$datafile' does not exist" unless -f $datafile;
   my $limit_s        = $$task{limit_s};     die "Limit is unset" unless defined $limit_s;
   my $ann_search     = $$task{search};      # may be unset, otherwise a hash with name and value
   my $ann_restart    = $$task{restart};     # may be unset, otherwise a hash with name and value

   my $cmdline = build_minizinc_cmdline($modelfile,$datafile,$ann_search,$ann_restart,$limit_s);

   my $config_name      = $$task{config_name};      die "Configuration name is unset" unless $config_name;
   my $application_name = $$task{application_name}; die "Application name is unset" unless $application_name;
   my $resultfile       = $$task{resultfile};       die "Result file '$resultfile' does not exist" unless -f $resultfile;
   my $modelfile_base   = $$task{modelfile_base};   die "Base for modelfile is unset" unless $modelfile_base;  # For naming the result line
   my $datafile_base    = $$task{datafile_base};    die "Base for datafile is unset" unless $datafile_base;   # For naming the result line
   my $round            = $$task{round};            die "Round is unset" unless defined $round;         # For naming the result line
   my $rounds           = $$task{rounds};           die "Rounds is unset" unless defined $rounds;       # For naming the result line
   my $objval_name      = $$task{objval_name};      die "Objective value name is unset" unless defined $objval_name;

   my ($fh_out,$filename_out,$fh_err,$filename_err) = create_logfiles($modelfile_base,$datafile_base,$config_name,$application_name,$round,$logs_dir);

   write_header_comment($fh_out,$task);
   write_header_comment($fh_err,$task);

   if ($add_cmdline_to_logs) {
      write_cmdline($fh_out,$cmdline);
      write_cmdline($fh_err,$cmdline);
   }

   # Switch STDOUT and STDERR before starting the MiniZinc subprocess, so that the subprocess
   # will inhert filedescriptors pointing to the files just opened

   open(my $old_stdout, '>&', STDOUT); open(STDOUT, '>>&', $fh_out);
   open(my $old_stderr, '>&', STDERR); open(STDERR, '>>&', $fh_err);

   # Start process, wait then restore STDOUT and STDERR

   ## >>>>>
   my $start  = time();
   my $retval = system(@$cmdline);
   my $end    = time();
   ## <<<<<<

   open(STDOUT, '>&', $old_stdout) || die "Could not restore STDOUT: $!";
   open(STDERR, '>&', $old_stderr) || die "Could not restore STDERR: $!";

   close($fh_out) || die "Could not close $filename_out: $!";
   close($fh_err) || die "Could not close $filename_err: $!";

   process_system_retval_and_maybe_exit($retval);

   my $sol;
   my $mzn;
   my $around;
   my $seq;

   {
      ($sol,$mzn) = extract_solution_and_stats($filename_out,$objval_name);
      $around = { duration_s => ($end-$start) , limit_s => $limit_s };
      # TODO: Flexibilize this.
      # Currently only useful for a specific MiniZinc model, namely one
      # in which the objval_id is printed followed by a (nameless) array of integers
      $seq = extract_sequence($filename_out,$objval_name,$mzn)
   }

   if ($debug_results) {
      print STDERR Data::Dumper->new({sol => $sol, mzn => $mzn,around => $around,seq => $seq})->Sortkeys(1)->Dump;
   }

   write_to_resultfile_outer($task,$sol,$mzn,$around);

   # If all went well, delete the files that captured stdout and stderr

   if ($keep_logs) {
      open(my $fh, '>>', $filename_out) or die "Could not open '$filename_out' for writing";
      print $fh Data::Dumper->new([$mzn])->Sortkeys(1)->Dump . "\n";
      close($fh);
   }
   else {
      unlink $filename_out || die "Could not unlink '$filename_out'";
      unlink $filename_err || die "Could not unlink '$filename_err'"
   }
}

sub create_logfiles {
   my($modelfile_base,$datafile_base,$config_name,$application_name,$round,$logs_dir) = @_;
   my $filename_base = "${modelfile_base}_${datafile_base}_${config_name}_${application_name}_${round}";
   my $filename_out = File::Spec->catfile($logs_dir,"${filename_base}.out");
   my $filename_err = File::Spec->catfile($logs_dir,"${filename_base}.err");
   my $fh_out;
   my $fh_err;
   open($fh_out,">",$filename_out) or die "Could not create logfile '$filename_out' for writing: $!";
   open($fh_err,">",$filename_err) or die "Could not create logfile '$filename_err' for writing: $!";
   binmode( $fh_out, ":utf8" );
   binmode( $fh_err, ":utf8" );
   return ($fh_out, $filename_out, $fh_err, $filename_err)
}

# The "cmdline" is an array that will be given to Perl's "system" call
# which forks a new child process and that process then execs the given
# program (i.e. "minizinc", which must be on the path) with the given
# argument vector (this does not pass through a shell interpreter)
#
# https://www.minizinc.org/doc-2.5.5/en/command_line.html
# https://perldoc.perl.org/functions/system
#
# For predefined commandline options "known to Gecode" (i.e. known
# to a program that uses the "script commandline driver" provided
# by the Gecode library), see the Gecode manual "Modeling and
# Programming with Gecode" on page 198 of https://www.gecode.org/doc-latest/MPG.pdf
#
# The passed $params is a hash, which contains:
#
# limit_s
# -------
# maps to an integer > 0 the "cutoff time in seconds".
# This needs to be transformed to milliseconds for gecode.
#
# If "limit_s" is missing (it should not) or its value is <= 0, then we have
# "no cutoff" i.e. the search continues until the whole tree has been traversed.
#
# ann_search
# ----------
# A subhash with:
#
# name  : name of the search annotation in the model file
# value : full text of the annotation
#
# If "ann_search" is missing, then we assume that there is no search
# annotation placeholder to be filled in in the model file.
#
# ann_restart
# -----------
# A subhash with:
#
# name  : name of the search annotation in the model file
# value : full text of the annotation
#
# If "ann_restart" is missing, then we assume that there is no search
# annotation placeholder to be filled in in the model file.
# Note that the same effect can be achieved by setting "value" to
# "restart_none".
# Alternatively, one can use specialized gecode parameters to
# configure restarts, -restart, -restart-scale and -restart-base.

sub build_minizinc_cmdline {
   my($modelfile,$datafile,$ann_search,$ann_restart,$limit_s) = @_;
   die "Modelfile must be given" unless $modelfile;
   die "Datafile must be given" unless $datafile;
   my $cmdline = [
      "minizinc"
      , "--statistics"
      , "--solver"    , "Gecode"
      , "--model"     , $modelfile
      , "--data"      , $datafile
   ];
   maybe_add_gecode_time_limit_parameter($cmdline,$limit_s);
   maybe_add_annotation($cmdline,$ann_search,"search");
   maybe_add_annotation($cmdline,$ann_restart,"restart");
   return $cmdline
}

sub maybe_add_annotation {
   my($cmdline,$ann,$type) = @_;
   my($set,$name,$value) = extract_annotation($ann,$type);
   if ($set) {
      push @$cmdline, "--cmdline-data", "$name = $value";
   }
}

sub extract_annotation {
   my($ann,$type) = @_;
   my $name;
   my $value;
   my $set = 0;
   if (defined $ann && %$ann > 0) {
      $name = $$ann{name};
      $value = $$ann{value};
      $set = 1;
      die "Name of $type annotation is not defined" unless $name;
      die "Value of $type annotation is not defined" unless $value
   }
   return ($set,$name,$value)
}

# Add a tuple for the "time limit" parameter to the cmdline
# argument array (i.e. modify in-place) if the passed "$limit_s" is defined
# and > 0.
#
# Jip J. Dekker says:
#
# "The --fznflags MiniZinc flag is used to pass flags directly to the solver,
#  in newer versions of MiniZinc this is not necessary anymore. Instead for
#  solvers that have a correct configuration you can just use the flags directly."
#
# In this case, "--time" (also written as "-time") is "cutoff for time in
# milliseconds". See page 198 of the PDF manual for Gecode, "Modeling and
# Programming with Gecode", available here:
# https://www.gecode.org/doc/6.2.0/reference/index.html

sub maybe_add_gecode_time_limit_parameter {
   my($cmdline,$limit_s) = @_;
   if (defined $limit_s && $limit_s*1 > 0) {
      my $val_ms = int($limit_s*1000);
      # Old school (works)
      push @$cmdline, "--fzn-flags" , "--time $val_ms";
      # New school (not working!)
      #   N.B. "-time" (a mix between 'long options' and POSIX 'single dash')
      #   not "--time" (double dash GNU convention for 'long options')
      # push @$cmdline, "-time", "$val_ms";
   }
}

sub write_header_comment {
   my($fh,$task) = @_;
   my $modelfile        = $$task{modelfile};
   my $datafile         = $$task{datafile};
   my $limit_s          = $$task{limit_s};
   my $ann_search       = $$task{search};
   my $ann_restart      = $$task{restart};
   my $config_name      = $$task{config_name};
   my $application_name = $$task{application_name};
   my $round            = $$task{round};
   print $fh "% now         : " . strftime("%F %T", localtime time) . "\n";
   print $fh "% modelfile   : $modelfile\n";
   print $fh "% datafile    : $datafile\n";
   print $fh "% config      : $config_name\n";
   print $fh "% application : $application_name\n";
   print $fh "% limit_s     : $limit_s\n";
   print $fh "% round       : $$task{round} of $$task{rounds}\n";
   if ($ann_search) {
      print $fh "% search annotation name   : $$ann_search{name}\n";
      print $fh "% search annotation value  : $$ann_search{value}\n"
   }
   if ($ann_restart) {
      print $fh "% restart annotation name  : $$ann_restart{name}\n";
      print $fh "% restart annotation value : $$ann_restart{value}\n"
   }
}

sub write_cmdline {
   my($fh,$cmdline) = @_;
   print $fh "% ";
   for my $str (@$cmdline) {
      print $fh " ";
      if ($str =~ /[\s\"]/) {  # a vague attempt at quoting
         print $fh "'$str'";
      }
      else {
         print $fh $str;
      }
   }
   print $fh "\n";
}

sub process_system_retval_and_maybe_exit {
   my($retval) = @_;
   if ($retval == -1) {
      print STDERR "Failed to execute MiniZinc: $!\n";
      exit 1
   }
   elsif ($retval & 127) {
      my $sigval = ($retval & 127);
      printf STDERR "MiniZinc died due to signal " . $sigval . ", " . (($retval & 128) ? "with" : "without") . " coredump\n";
      exit 1
   }
   else {
      my $exitval = ($retval >> 8);
      if ($exitval > 0) {
         printf STDERR "MiniZinc exited with value $exitval\n";
         exit 1
      }
   }
}

# Solution data will be stored in a subhash under "solution", MiniZinc performance
# data will be stored directly in the "res" hash.
#
# The program objective value is expected on a single line
# <objval_id> = <numeric>;
# This could be flexibilized!

sub extract_solution_and_stats {
   my($file,$objval_name) = @_;
   open(my $fh,"<",$file) || die "Could not open file '$file' for reading: $!";
   my $mzn = {};
   my $sol = {};
   while (my $line = <$fh>) {
      chomp $line;
      if ($line =~ /^\s*${objval_name}\s*=\s*([\d\.]+)\s*;?\s*$/) {
         my $numeric = $1*1;
         print STDERR "Found $objval_name = $1 in '$line'; converted to numeric: $numeric\n";
         $$sol{objval_value} = $numeric;
         $$sol{objval_name}  = $objval_name;
         next
      }
      if ($line =~ /^%%%mzn-stat: initTime=([\d\.]+)\s*$/)     { $$mzn{init_time_s}   = $1*1 ; next }
      if ($line =~ /^%%%mzn-stat: solveTime=([\d\.]+)\s*$/)    { $$mzn{solve_time_s}  = $1*1 ; next }
      if ($line =~ /^%%%mzn-stat: solutions=(\d+)\s*$/)        { $$mzn{solutions}     = $1*1 ; next }
      if ($line =~ /^%%%mzn-stat: variables=(\d+)\s*$/)        { $$mzn{variables}     = $1*1 ; next }
      if ($line =~ /^%%%mzn-stat: propagators=(\d+)\s*$/)      { $$mzn{propagators}   = $1*1 ; next }
      if ($line =~ /^%%%mzn-stat: propagations=(\d+)\s*$/)     { $$mzn{propagations}  = $1*1 ; next }
      if ($line =~ /^%%%mzn-stat: nodes=(\d+)\s*$/)            { $$mzn{nodes}         = $1*1 ; next }
      if ($line =~ /^%%%mzn-stat: failures=(\d+)\s*$/)         { $$mzn{failures}      = $1*1 ; next }
      if ($line =~ /^%%%mzn-stat: restarts=(\d+)\s*$/)         { $$mzn{restarts}      = $1*1 ; next }
      if ($line =~ /^%%%mzn-stat: peakDepth=(\d+)\s*$/)        { $$mzn{peak_depth}    = $1*1 ; next }
      if ($line =~ /^%%%mzn-stat: nSolutions=(\d+)\s*$/)       { $$mzn{num_solutions} = $1*1 ; next }
   }
   close($fh);
   return ($sol,$mzn);
}

sub parse_sequence {
   my($line) = @_;
   my $res = {};
   my $counter = 1;
   my $rest = $line;
   while (!($rest =~ /^\s*$/)) {
      if ($rest =~ /^\s*(\d+)\b(.*)$/) {
         $$res{$counter} = $1*1;
         $counter++;
         $rest = $2;
      }
      else {
         die "Bad rest '$rest' of line '$line'";
      }
   }
   return $res
}

# Very specific to this output generation:
#
# output ["makespan = ", show(makespan), "\n" ] ++
#        [ show_int(3,s[t]) ++ " "
#        | t in TASK];
#
# The objval_id (in this case, "makespan") is only passed because we need
# to look for the line following its occurrence.

sub extract_sequence {
   my($file,$objval_name,$res) = @_;
   open(my $fh,"<",$file) || die "Could not open file '$file': $!";
   my $seq;
   my $expect_sequence = 0;
   while (my $line = <$fh>) {
      chomp $line;
      if ($expect_sequence) {
         $seq = parse_sequence($line);
         $expect_sequence = 0;
      }
      else {
         if ($line =~ /^\s*${objval_name}\s*=\s*([\d\.]+)\s*;?\s*$/) {
            # The sequence is expected to come after the "obj" line
            $expect_sequence = 1;
            next
         }
      }
   }
   close($fh);
   return $seq
}

# write out a CSV line for a hash keyed up as defined in get_resultfile_line()

sub write_to_resultfile_outer {
   my($task,$sol,$mzn,$around) = @_;
   my $result = {
       model                    => $$task{modelfile_base}
      ,data                     => $$task{datafile_base}
      ,config_name              => $$task{config_name}
      ,application_name         => $$task{application_name}
      ,round                    => $$task{round}
      ,rounds                   => $$task{rounds}
      ,limit_s                  => $$around{limit_s}
      ,duration_s               => $$around{duration_s}
      ,obj                      => $$sol{objval_value}
      ,'minizinc.init_time_s'   => $$mzn{init_time_s}
      ,'minizinc.solve_time_s'  => $$mzn{solve_time_s}
      ,'minizinc.solutions'     => $$mzn{solutions}
      ,'minizinc.variables'     => $$mzn{variables}
      ,'minizinc.propagators'   => $$mzn{propagators}
      ,'minizinc.propagations'  => $$mzn{propagations}
      ,'minizinc.nodes'         => $$mzn{nodes}
      ,'minizinc.failures'      => $$mzn{failures}
      ,'minizinc.restarts'      => $$mzn{restarts}
      ,'minizinc.peak_depth'    => $$mzn{peak_depth}
      ,'minizinc.num_solutions' => $$mzn{num_solutions}
   };
   {
      my ($set,$name,$value) = extract_annotation($$task{search},"search");
      $$result{search} = ($set ? $value : "NA");
   }
   {
      my ($set,$name,$value) = extract_annotation($$task{restart},"restart");
      $$result{restart} = ($set ? $value : "NA");
   }
   # writer dies on error
   write_to_resultfile($$task{resultfile},$result);
}

