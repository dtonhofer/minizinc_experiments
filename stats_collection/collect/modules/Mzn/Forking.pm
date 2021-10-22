#!/usr/bin/perl

######
# A module for a program which runs MiniZinc repeatedly on the same model with
# different data files and different search annotations (to apply different
# variable selection strategies and domain splitting strategies), and collects
# information about the run in a CSV file
#
# This module deals with running MiniZinc and collecting results.
#
# The homepage for this script with additional explanation is at
# https://github.com/dtonhofer/minizinc_experiments/tree/main/stats_collection
######

package Mzn::Forking;

use warnings;
use strict;
use utf8;  # Meaning "This lexical scope (i.e. file) contains utf8"

use Data::Dumper; # install with "dnf install perl-Data-Dumper"
use File::Temp qw(tempfile tempdir);
use List::Util qw(max min);
use POSIX qw(strftime);

use Mzn::ResultFiling
   qw(write_to_resultfile);

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

   my $datafile       = $$task{datafile};    die "Data file '$datafile' does not exist" unless -e $datafile;
   my $modelfile      = $$task{modelfile};   die "Model file '$modelfile' does not exist" unless -e $modelfile;
   my $resultfile     = $$task{resultfile};  die "Result file '$resultfile' does not exist" unless -e $resultfile;
   my $base_df        = $$task{base_df};     die "Base for datafile is unset" unless $base_df;   # For naming the result line
   my $base_mf        = $$task{base_mf};     die "Base for modelfile is unset" unless $base_mf;  # For naming the result line
   my $round          = $$task{round};       die "Round is unset" unless defined $round; # For naming the result line
   my $rounds         = $$task{rounds};      die "Rounds is unset" unless defined $rounds; # For naming the result line
   my $annotation     = $$task{annotation};  die "Annotation is unset" unless defined $annotation; # TODO: We may need no annotation!
   my $ann_name       = $$task{ann_name};    die "Annotation name is unset" unless defined $ann_name; # TODO: We may need no annotation!
   my $obj_name       = $$task{obj_name};    die "Objective value name is unset" unless defined $ann_name;
   my $limit_s        = $$task{limit_s};     # Could be unset

   my ($fh_out,$filename_out,$fh_err,$filename_err) = create_tempfiles($base_mf,$base_df,$round,$logs_dir);

   my $cmdline = build_minizinc_cmdline($modelfile,$datafile,$ann_name,$annotation,$limit_s);

   write_header_comment($fh_out,$task,"$ann_name = $annotation",$limit_s);
   write_header_comment($fh_err,$task,"$ann_name = $annotation",$limit_s);

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

   my $mzn_res = extract_solution_and_stats($filename_out,$obj_name);

   # TODO: Flexibilize this. Currently only useful for a specific MiniZinc model
   extract_sequence($filename_out,$obj_name,$mzn_res);

   $$mzn_res{around} = {
       duration_s => ($end-$start)
      ,limit_s    => $limit_s
   };

   if ($debug_results) {
      print STDERR Data::Dumper->new([$mzn_res])->Sortkeys(1)->Dump;
   }

   write_to_resultfile_outer($mzn_res,$task);

   # If all went well, delete the files that captured stdout and stderr

   if ($keep_logs) {
      open(my $fh, '>>', $filename_out) or die "Could not open '$filename_out' for writing";
      print $fh Data::Dumper->new([$mzn_res])->Sortkeys(1)->Dump . "\n";
      close($fh);
   }
   else {
      unlink $filename_out || die "Could not unlink '$filename_out'";
      unlink $filename_err || die "Could not unlink '$filename_err'"
   }
}

# Use https://perldoc.perl.org/File::Temp to create files
# in directory "logs" to cpature stderr and stdout of MiniZinc
# process.

sub create_tempfiles {
   # Need at least 4 X as "random char placeholder", can we get rid of those
   my($base_mf,$base_df,$round,$logs_dir) = @_;
   my $tempfile_pattern = "${base_mf}_${base_df}_${round}_XXXX";
   (my ($fh_out, $filename_out) =
      tempfile( $tempfile_pattern ,
                SUFFIX => ".out",
                DIR => $logs_dir )) ||
      die "Could not create temp file: $!";
   (my ($fh_err, $filename_err) =
      tempfile( $tempfile_pattern ,
                SUFFIX => ".err",
                DIR => $logs_dir )) ||
      die "Could not create temp file: $!";
   # print STDERR "Filename_out = $filename_out\n";
   # print STDERR "Filename_err = $filename_err\n";
   binmode( $fh_out, ":utf8" );
   binmode( $fh_err, ":utf8" );
   return ($fh_out, $filename_out, $fh_err, $filename_err)
}

# The cmdline is an array that will be given to Perl's "system" call
# https://www.minizinc.org/doc-2.5.5/en/command_line.html
# https://perldoc.perl.org/functions/system

sub build_minizinc_cmdline {
   my($modelfile,$datafile,$ann_name,$annotation,$limit_s) = @_;
   my $cmdline = [
      "minizinc",                                       # must be on path
      "--statistics",
      "--solver"         , "Gecode",                    # TODO: parametrize
      "--model"          , $modelfile,
      "--data"           , $datafile,
      "--cmdline-data"   , "$ann_name = $annotation"
   ];
   # TODO: How to pass "restart" to gecode here?
   maybe_add_time_limit_parameter($cmdline,$limit_s);
   return $cmdline
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

sub maybe_add_time_limit_parameter {
   my($cmdline,$limit_s) = @_;
   if (defined($limit_s) && $limit_s > 0) {
      my $val_ms = max(1000,$limit_s*1000); # at least 1000 ms
      push @$cmdline, "--fzn-flags" , "--time $val_ms";
      # push @$cmdline, "--time", "$val_ms"; # not actually working
   }
}

sub write_header_comment {
   my($fh,$task,$ann_desc,$limit_s) = @_;
   print $fh "% now        = " . strftime("%F %T", localtime time) . "\n";
   print $fh "% modelfile  = $$task{modelfile}\n";
   print $fh "% datafile   = $$task{datafile}\n";
   print $fh "% round      = $$task{round} of $$task{rounds}\n";
   print $fh "% limit_s    = $limit_s\n";
   print $fh "% annotation = $ann_desc\n";
}

sub write_cmdline {
   my($fh,$cmdline) = @_;
   print $fh "% ";
   for my $str (@$cmdline) {
      print $fh " ";
      if ($str =~ /[\s\"]/) {  # an attempt at quoting
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
# <obj_name> = <numeric>;
# This could be flexibilized!

sub extract_solution_and_stats {
   my($file,$obj_name) = @_;
   open(my $fh,"<",$file) || die "Could not open file '$file' for reading: $!";
   my $res = {};
   $$res{solution} = {};
   $$res{minizinc} = {};
   my $mzn = $$res{minizinc};
   my $sol = $$res{solution};
   while (my $line = <$fh>) {
      chomp $line;

      if ($line =~ /^\s*${obj_name}\s*=\s*([\d\.]+)\s*;?\s*$/) {
         my $numeric = $1*1;
         print STDERR "Found $obj_name = $1 in '$line'; numeric: $numeric\n";
         $$sol{obj_value} = $numeric;
         $$sol{obj_name}  = $obj_name;
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
   return $res
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

sub extract_sequence {
   my($fq_file,$obj_name,$res) = @_;
   open(my $fh,"<",$fq_file) || die "Could not open file '$fq_file': $!";
   my $expect_sequence = 0;
   while (my $line = <$fh>) {
      chomp $line;
      if ($expect_sequence) {
         $$res{solution}{sequence} = parse_sequence($line);
         $expect_sequence = 0;
      }
      else {
         if ($line =~ /^\s*${obj_name}\s*=\s*([\d\.]+)\s*;?\s*$/) {
            # The sequence is expected to come after the "obj" line
            $expect_sequence = 1;
            next
         }
      }
   }
   close($fh);
}

sub write_to_resultfile_outer {
   my($data,$task) = @_;
   my $around   = $$data{around};
   my $mzn      = $$data{minizinc};
   my $solution = $$data{solution};
   my $result = {
       model                    => $$task{base_mf}
      ,data                     => $$task{base_df}
      ,round                    => $$task{round}
      ,rounds                   => $$task{rounds}
      ,limit_s                  => $$around{limit_s}
      ,annotation               => $$task{annotation}
      ,duration_s               => $$around{duration_s}
      ,obj                      => $$solution{obj_value}
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
   # writer dies on error
   write_to_resultfile($$task{resultfile},$result);
}

