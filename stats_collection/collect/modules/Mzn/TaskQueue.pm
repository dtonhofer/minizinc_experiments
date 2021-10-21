#!/usr/bin/perl

######
# A module for a program which runs MiniZinc repeatedly on the same model with
# different data files and different search annotations (to apply different
# variable selection strategies and domain splitting strategies), and collects
# information about the run in a CSV file
#
# This module deals with setting up the "task queue" and some extras.
#
# The homepage for this script with additional explanation is at
# https://github.com/dtonhofer/minizinc_experiments/tree/main/stats_collection
######

package Mzn::TaskQueue;

use strict;
use warnings;
use utf8;  # Meaning "This lexical scope (i.e. file) contains utf8"

use YAML::Tiny;     # install with "dnf install perl-YAML-Tiny"
use Data::Dumper;   # install with "dnf install perl-Data-Dumper"
use File::Basename;
use Fcntl qw(:flock SEEK_END);

use Exporter qw(import);
our @EXPORT_OK =
   qw(
       build_task_queue
       scramble_task_queue
       create_logs_dir_if_missing
     );

sub create_logs_dir_if_missing {
   my($logs_dir) = @_;
   if (! -d $logs_dir) {
      mkdir $logs_dir || die "Could not create the logs directory '$logs_dir': $!"
   }
}

sub build_task_queue {
   my($args) = @_;
   my $task_queue = [];
   my $modelfiles_array = $$args{modelfiles};
   for my $modelfile (sort @$modelfiles_array) {
      my $datafiles_hash = $$args{datafiles};
      for my $datafile (sort keys %$datafiles_hash) {
         my $config_name = $$datafiles_hash{$datafile}; die "Nothing under '$datafile'" unless $config_name;
         my $resultfile  = $$args{resultfile};
         my $configs     = $$args{configs};
         add_to_task_queue($task_queue,$modelfile,$datafile,$resultfile,$config_name,$configs)
      }
   }
   if ($$args{debug_config}) {
      print STDERR "There are " . scalar(@$task_queue) . " entries in the task queue!\n";
   }
   return $task_queue
}

sub add_to_task_queue {
   my($task_queue,$modelfile,$datafile,$resultfile,$config_name,$configs) = @_;
   my $config       = $$configs{$config_name}; die "No config for '$config_name'" unless defined $config;
   my $strategies   = $$config{strategies};    die "No strategies for '$config_name'" unless defined $strategies;
   for my $strategy_name (sort keys %$strategies) {
      my $strategy_hash = $$strategies{$strategy_name};
      my $rounds        = $$strategy_hash{rounds};     # should be set and >= 0 (0 meaning no rounds)
      my $limit_s       = $$strategy_hash{limit_s};    # should be set and >= 0 (0 meaning no limit)
      my $annotation    = $$strategy_hash{annotation};
      my $ann_name      = $$strategy_hash{ann_name};
      my $obj_name      = $$strategy_hash{obj_name};
      for (my $round = 1; $round <= $rounds; $round++) { # 1-based for once
         my $base_df = basename($datafile);
         my $base_mf = basename($modelfile);
         if ($base_df =~ /^(.+)\.dzn$/) { $base_df = $1 }
         if ($base_mf =~ /^(.+)\.mzn$/) { $base_mf = $1 }
         push @$task_queue,
            { datafile      => $datafile,
              resultfile    => $resultfile,
              modelfile     => $modelfile,
              base_mf       => $base_mf,
              base_df       => $base_df,
              round         => $round,           # 1...rounds
              rounds        => $rounds,          # constant
              ann_name      => $ann_name,
              annotation    => $annotation,
              limit_s       => $limit_s,
              obj_name      => $obj_name }
      }
   }
}

sub scramble_task_queue {
   my($task_queue) = @_;
   my $tmp_task_queue = [@$task_queue]; # obtain reference to a copy
   my $res_task_queue = [];
   while (@$tmp_task_queue > 0) {
      my $index = int(rand(@$tmp_task_queue)); # between 0 and length of @$tmp_task_queue exclusive
      my $task  = splice(@$tmp_task_queue,$index,1);
      push @$res_task_queue, $task;
   }
   return $res_task_queue;
}


