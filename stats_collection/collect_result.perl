#!/usr/bin/perl

use warnings;
use strict;

use Fcntl qw(:flock SEEK_END);
use File::Temp qw(tempfile tempdir);
use File::Basename;
use List::Util qw(min max);

# Variable selection straegies as requested in the workshop

my $var_sels = [ 
    'input_order'
   ,'first_fail' 
   ,'smallest' 
   ,'largest' 
];

# Value selection strategies as requested in the workshop

my $val_sels = [ 
    'indomain_min'
   ,'indomain_max'
   ,'indomain_median'
   ,'indomain_random'
   ,'indomain_split'
   ,'indomain_reverse_split'
];

# How many rounds to do for each problem (so as to accumulate a few values)
# This may enable us to add some error bars later.

my $rounds = 5;

# Processing time limit for optimization (in seconds) 

my $limit_s = 60;

# List the data files and for each, give a time limit in seconds and a number of rounds.
# 0 rounds means don't run this.

my $data_files = {
   'prepare1.dzn'  => { limit_s => $limit_s, rounds => $rounds },
   'prepare2.dzn'  => { limit_s => $limit_s, rounds => $rounds },
   'prepare3.dzn'  => { limit_s => $limit_s, rounds => $rounds },
   'prepare4.dzn'  => { limit_s => $limit_s, rounds => $rounds },
   'prepare5.dzn'  => { limit_s => $limit_s, rounds => $rounds },
   'prepare6.dzn'  => { limit_s => $limit_s, rounds => $rounds },
   'prepare7.dzn'  => { limit_s => $limit_s, rounds => $rounds },
   'prepare8.dzn'  => { limit_s => $limit_s, rounds => $rounds },
   'prepare9.dzn'  => { limit_s => $limit_s, rounds => $rounds },
   'prepare10.dzn'  => { limit_s => $limit_s, rounds => $rounds },
   'prepare11.dzn'  => { limit_s => $limit_s, rounds => $rounds },
   'prepare12.dzn'  => { limit_s => $limit_s, rounds => $rounds },
   'prepare13.dzn'  => { limit_s => $limit_s, rounds => $rounds },
   'prepare14.dzn'  => { limit_s => $limit_s, rounds => $rounds }
};


# ---
# Establish directory where our stuff is, relative to this executable.
#
#                $work_dir/
#                    |
#      +-------------+-----------------+-------------+-----------------+
#      |             |                 |             |                 |
#  prog.perl   $fq_data_dir/    $fq_model_file   $fq_log_dir/     $fq_result_file
#                    |             (mzn file)        |              (CSV file)
#                   / \                             / \
#              (contains the                     (accumulates
#               @$data_files)                  subprocess logs)
#
# ---

my $work_dir       = dirname($0); # this doesn't necessarily work, need to process a --work_dir=... arg
my $fq_model_file  = "$work_dir/prepare_modded.mzn"; 
my $fq_data_dir    = "$work_dir/data";
my $fq_log_dir     = "$work_dir/log";
my $fq_result_file = "$work_dir/result.txt";

die "Work directory '$work_dir' does not exist" unless (-d $work_dir);
die "Modelfile '$fq_model_file' does not exist" unless (-e $fq_model_file);
die "Directory '$fq_data_dir' does not exist"   unless (-d $fq_data_dir);

if (! -d $fq_log_dir) {
   mkdir $fq_log_dir || die "Could not create the log directory '$fq_log_dir': $!"
}

if (-e $fq_result_file) {
   print STDERR "The result file '$fq_result_file' exists -- renaming it!\n";
   my $index = 1;
   my $bak_name;
   do {
      $bak_name = "${fq_result_file}.${index}";
      $index++;
   } until (! -e $bak_name);
   rename($fq_result_file,$bak_name) || die "Could not rename file '$fq_result_file' to '$bak_name': $!";
   print "Renamed the existing result file to '$bak_name'\n";
}

my $task_queue   = [];     # collects hashes describing minizinc tasks to be run
my $children     = {};     # map pid of a running child process to 1; once empty, all the child processes are done
my $max_children = 4;      # on this machine, there are four cores, so run at most 4 children at a time

# ---

sub fill_task_queue {
   for my $data_file (keys %$data_files) {
      if ($data_file =~ /^([.a-zA-Z0-9_-]+)\.dzn$/) {
         my $base = $1;
         my $fq_data_file = "$fq_data_dir/$data_file";
         if ( -e $fq_data_file) {
            fill_task_queue_for_file($fq_data_file,$data_file,$base)
         }
         else {
            print STDERR "The data file '$fq_data_file' does not exist -- skipping that file!\n";
         }
      }
      else {
         print STDERR "The supposed data file '$data_file' contains bad characters in the base name or does not end in .dzn -- skipping that file!\n";
      }
   }
}

sub fill_task_queue_for_file {
   my($fq_data_file,$data_file,$base) = @_;
   my $sub_hash = $$data_files{$data_file};
   my $rounds   = $$sub_hash{rounds}; die "Bad 'rounds' for '$data_file'" unless (defined($rounds) && $rounds >= 0); 
   for (my $round = 0; $round < $rounds; $round++) {
      for my $var_sel (@$var_sels) {
         for my $val_sel (@$val_sels) {
            push @$task_queue,
               { fq_data_file   => $fq_data_file,
                 fq_result_file => $fq_result_file,  # from global variable
                 fq_model_file  => $fq_model_file,   # from global variable
                 base           => $base,
                 round          => $round,
                 var_sel        => $var_sel,
                 val_sel        => $val_sel,
                 limit_s        => $$sub_hash{limit_s} }
         }
      }
   }
}

sub scramble_task_queue_in_place_to_inject_randomness {
   my $new_task_queue = [];
   while (@$task_queue > 0) {
      my $index = int(rand(@$task_queue)); # between 0 and length of @$task_queue exclusive
      my $task  = splice(@$task_queue,$index,1);
      push @$new_task_queue, $task;
   }
   $task_queue = $new_task_queue;
}

fill_task_queue();
scramble_task_queue_in_place_to_inject_randomness();

print STDERR "There are " . scalar(@$task_queue) . " entries in the task queue!\n";

sub touch_result_file {
   open(my $fh,">",$fq_result_file) or die "Could not open file '$fq_result_file' for output: $!";
   print $fh "base";
   print $fh ", round";
   print $fh ", var_sel"; 
   print $fh ", val_sel";
   print $fh ", limit_s";
   print $fh ", duration_s";
   print $fh ", makespan";
   print $fh ", init_time_s";
   print $fh ", solve_time_s"; 
   print $fh ", solutions";
   print $fh ", variables";
   print $fh ", propagators";
   print $fh ", propagations";
   print $fh ", nodes";
   print $fh ", failures";
   print $fh ", restarts";
   print $fh ", peak_depth";
   print $fh ", num_solutions\n";
   close($fh) or die "Could not close file '$fq_result_file': $!";
}

touch_result_file();

sub main_loop {
   while (@$task_queue > 0) {
      if (%$children >= $max_children) {
         print STDERR "Waiting for an empty child slot...\n";
         wait_for_child_termination();
         die "There should be free slots now" unless %$children < $max_children
      }
      my $task = shift @$task_queue;
      print "Submitting work. There are now " . scalar(@$task_queue) . " entries left in the task queue.\n";
      ## >>>
      fork_child_process($task)
      ## <<< 
   }
}

sub wait_for_child_termination() {
   my $pid = wait(); # wait for any child
   if ($pid == -1) {
      print STDERR "No more child processes\n"
   }
   else {
      my $retval = $?;
      # Not sure whether the lower 8 bit are set after wait (?)
      if ($retval & 127) {
         my $sigval = ($retval & 127);
         printf STDERR "Child process with PID $pid died due to signal " . $sigval . ", " . (($retval & 128) ? "with" : "without") . " coredump\n";
      }
      else {
         # Good termination, see what the process said
         my $exit_status = ($retval >> 8);
         if ($exit_status > 0) {
            print STDERR "Child process with PID $pid terminated with non-zero exit status $exit_status\n";
         }
      }
      die "There is no child with PID $pid in the children hash!" unless exists $$children{$pid};
      delete $$children{$pid}
   }
}

sub reap_all_children {
   while (%$children > 0) {
      wait_for_child_termination()
   }
}

sub fork_child_process {
   my($task) = @_;
   # >>>>>>>>
   my $pid = fork();
   # <<<<<<<<
   if (!defined($pid)) {
      die "Fork of child process was unsuccessful: $!"
   }
   elsif ($pid > 0) {
      # This is the parent process!
      die "Child pid $pid already exists in the children hash!" if exists $$children{$pid};
      $$children{$pid} = 1;
   }
   else {
      # This is the child process - we can now start minizinc in here
      die unless ($pid == 0);
      fork_minizinc($task); # should never return
      exit 0; # make sure you don't get back into the main loop in this subprocess
   }
}

sub fork_minizinc {
   my($task) = @_;
   my $fq_data_file   = $$task{fq_data_file};    die "Data file '$fq_data_file' does not exist" unless -e $fq_data_file;
   my $fq_model_file  = $$task{fq_model_file};   die "Model file '$fq_model_file' does not exist" unless -e $fq_model_file;
   my $fq_result_file = $$task{fq_result_file};  die "Result file '$fq_result_file' does not exist" unless -e $fq_result_file;
   my $base           = $$task{base};            die "Base is unset" unless $base;   # For naming the result line
   my $round          = $$task{round};           die "Round is unset" unless defined $round; # For naming the result line
   my $s_annotation   = build_search_annotation_for_s($task);

   #
   # MiniZinc shall write its temporary files (one for stdout and another for stderr)
   #

   my $pid = $$;
   (my ($fh_out, $filename_out) = tempfile( "minizinc_${pid}_XXXX" , SUFFIX => ".out", DIR => $fq_log_dir )) || die "Could not create temp file: $!";
   (my ($fh_err, $filename_err) = tempfile( "minizinc_${pid}_XXXX" , SUFFIX => ".err", DIR => $fq_log_dir )) || die "Could not create temp file: $!";

   # print STDERR "Filename_out = $filename_out\n";
   # print STDERR "Filename_err = $filename_err\n";
   binmode( $fh_out, ":utf8" );
   binmode( $fh_err, ":utf8" );
   #
   # Build MiniZinc command line
   # https://www.minizinc.org/doc-2.5.5/en/command_line.html
   # https://perldoc.perl.org/functions/system
   #

    my $cmd = [ 
      "minizinc", 
      "--statistics", 
      "--solver"         , "Gecode", 
      "--model"          , $fq_model_file,
      "--data"           , $fq_data_file,
      "--cmdline-data"   , "s_annotation = $s_annotation"
   ];

   my $limit_s = maybe_add_time_limit_parameter($cmd,$task);

   write_header($fh_out,$task,$s_annotation,$limit_s);
   write_header($fh_err,$task,$s_annotation,$limit_s);

   # print STDERR "Now starting:\n"; for my $str (@$cmd) { print "$str\n"; }

   #
   # Switch STDOUT and STDERR before starting the MiniZinc subprocess, so that the subprocess
   # will inhert filedescriptors pointing to the files just opened
   #

   open(my $old_stdout, '>&', STDOUT); open(STDOUT, '>>&', $fh_out);
   open(my $old_stderr, '>&', STDERR); open(STDERR, '>>&', $fh_err);

   #
   # Start process, wait then restore STDOUT and STDERR
   #

   ## >>>>>
   my $start  = time();
   my $retval = system(@$cmd);
   my $end    = time();
   ## <<<<<<

   open(STDOUT, '>&', $old_stdout) || die "Could not restore STDOUT: $!";
   open(STDERR, '>&', $old_stderr) || die "Could not restore STDERR: $!";

   close($fh_out) || die "Could not close $filename_out: $!";
   close($fh_err) || die "Could not close $filename_err: $!";

   process_system_retval_and_maybe_exit($retval);

   my $mzn_res = extract_minizinc_solution_and_stats($filename_out);
 
   my $result = { duration_s => $end-$start , limit_s => $limit_s , minizinc => $mzn_res };

   write_to_result_file($task,$result);

   #
   # If all went well, delete the files that captured stdout and stderr
   #

   unlink $filename_out || die "Could not unlink '$filename_out'";
   unlink $filename_err || die "Could not unlink '$filename_err'";
}

sub write_header {
   my($fh,$task,$s_annotation,$limit_s) = @_;
   print $fh "% data         = " . basename($$task{fq_data_file}) . "\n";
   print $fh "% model        = " . basename($$task{fq_model_file}) . "\n";
   print $fh "% base         = $$task{base}\n";
   print $fh "% round        = $$task{round}\n";
   print $fh "% s_annotation = $s_annotation\n";
   print $fh "% limit_s      = $limit_s\n";
}

# The "search annotation"s apply to variable "s" (start values) only.
# Assume that if missing, the corresponding defaults apply.

sub build_search_annotation_for_s {
   my($task) = @_;
   my $var_sel = $$task{var_sel}; 
   my $val_sel = $$task{val_sel}; 
   if (!$var_sel) { $var_sel = 'input_order' }   # a default
   if (!$val_sel) { $val_sel = 'indomain_main' } # a default
   return "int_search(s, $var_sel, $val_sel)"
}

sub maybe_add_time_limit_parameter {
   my($cmd,$task) = @_;
   my $limit_s = $$task{limit_s};   
   if (defined($limit_s) && $limit_s > 0) {
      my $val = max(1000,$limit_s*1000);
      push @$cmd, "--fzn-flags" , "--time $val";
      return $limit_s
   }
   else {
     return 0
   }
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

sub write_to_result_file {
   my($task,$result) = @_;
   my $base           = $$task{base};
   my $round          = $$task{round};
   my $var_sel        = $$task{var_sel};
   my $val_sel        = $$task{val_sel};
   my $fq_result_file = $$task{fq_result_file};
   my $limit_s        = $$result{limit_s};
   my $duration_s     = $$result{duration_s};
   my $sequence       = $$result{minizinc}{solution}{sequence}; # a hash
   my $makespan       = $$result{minizinc}{solution}{makespan}; # a number
   my $init_time_s    = $$result{minizinc}{init_time_s};
   my $solve_time_s   = $$result{minizinc}{solve_time_s};
   my $solutions      = $$result{minizinc}{solutions};
   my $variables      = $$result{minizinc}{variables};
   my $propagators    = $$result{minizinc}{propagators};
   my $propagations   = $$result{minizinc}{propagations};
   my $nodes          = $$result{minizinc}{nodes};
   my $failures       = $$result{minizinc}{failures};
   my $restarts       = $$result{minizinc}{restarts};
   my $peak_depth     = $$result{minizinc}{peak_depth};
   my $num_solutions  = $$result{minizinc}{num_solutions};

   open(my $fh,">>",$fq_result_file) or die "Could not open file '$fq_result_file' for output: $!";
   lock($fh);

   printf $fh "%15s"     , $base;
   printf $fh ", %3d"    , $round;
   printf $fh ", %15s"   , $var_sel;
   printf $fh ", %25s"   , $val_sel;
   printf $fh ", %5d"    , $limit_s;
   printf $fh ", %5d"    , $duration_s;
   if (defined($makespan)) {
      printf $fh ", %4d"  , $makespan
   }
   else {
      printf $fh ",   NA"
   } 
   printf $fh ", %6.2f", $init_time_s;
   printf $fh ", %6.2f", $solve_time_s;
   printf $fh ", %4d"  , $solutions;
   printf $fh ", %4d"  , $variables;
   printf $fh ", %4d"  , $propagators;
   printf $fh ", %8d"  , $propagations;
   printf $fh ", %8d"  , $nodes;
   printf $fh ", %8d"  , $failures;
   printf $fh ", %4d"  , $restarts;
   printf $fh ", %4d"  , $peak_depth;
   printf $fh ", %2d"  , $num_solutions;
   print $fh "\n";

   unlock($fh);
   close($fh) or die "Could not close file '$fq_result_file': $!";
}

sub lock {
    my ($fh) = @_;
    flock($fh, LOCK_EX) or die "Cannot lock: $!\n";
}

sub unlock {
    my ($fh) = @_;
    flock($fh, LOCK_UN) or die "Cannot unlock: $!\n";
}

sub extract_sequence_of_start_times {
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


sub extract_minizinc_solution_and_stats {
   my($fq_file) = @_;
   open(my $fh,"<",$fq_file) || die "Could not open file '$fq_file': $!";

   my $res = {};
   $$res{solution} = {};
   my $expect_sequence = 0;

   while (my $line = <$fh>) {
      chomp $line;
      if ($expect_sequence) {
         $$res{solution}{sequence} = extract_sequence_of_start_times($line);
         $expect_sequence = 0;
      }
      else {
         if ($line =~ /^makespan\s*=\s*(\d+)\s*$/) {
            $$res{solution}{makespan} = $1*1;
            $expect_sequence = 1;
            next
         }
         if ($line =~ /^%%%mzn-stat: initTime=([\d\.]+)\s*$/)     { $$res{init_time_s}   = $1*1 ; next }
         if ($line =~ /^%%%mzn-stat: solveTime=([\d\.]+)\s*$/)    { $$res{solve_time_s}  = $1*1 ; next }
         if ($line =~ /^%%%mzn-stat: solutions=(\d+)\s*$/)        { $$res{solutions}     = $1*1 ; next }
         if ($line =~ /^%%%mzn-stat: variables=(\d+)\s*$/)        { $$res{variables}     = $1*1 ; next }
         if ($line =~ /^%%%mzn-stat: propagators=(\d+)\s*$/)      { $$res{propagators}   = $1*1 ; next }
         if ($line =~ /^%%%mzn-stat: propagations=(\d+)\s*$/)     { $$res{propagations}  = $1*1 ; next }
         if ($line =~ /^%%%mzn-stat: nodes=(\d+)\s*$/)            { $$res{nodes}         = $1*1 ; next }
         if ($line =~ /^%%%mzn-stat: failures=(\d+)\s*$/)         { $$res{failures}      = $1*1 ; next }
         if ($line =~ /^%%%mzn-stat: restarts=(\d+)\s*$/)         { $$res{restarts}      = $1*1 ; next }
         if ($line =~ /^%%%mzn-stat: peakDepth=(\d+)\s*$/)        { $$res{peak_depth}    = $1*1 ; next }
         if ($line =~ /^%%%mzn-stat: nSolutions=(\d+)\s*$/)       { $$res{num_solutions} = $1*1 ; next }
      }
   }
   close($fh);
   return $res
}




main_loop();
reap_all_children();



