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
   my $known_modelfiles = $$args{modelfiles};
   die "'known_modelfiles' should be an array" unless (ref $known_modelfiles eq ref []);
   if (@$known_modelfiles == 0) {
      print STDERR "No modelfiles!\n"
   }
   for my $modelfile (sort @$known_modelfiles) {
      btq_at_modelfile($task_queue,$modelfile,$args);
   }
   if ($$args{debug_config}) {
      print STDERR "There are " . scalar(@$task_queue) . " entries in the task queue!\n";
   }
   return $task_queue
}

sub btq_at_modelfile {
   my($task_queue,$modelfile,$args) = @_;
   my $known_datafiles = $$args{datafiles};
   die "'known_datafiles' should be a hash" unless (ref $known_datafiles eq ref {});
   if (%$known_datafiles == 0) {
      print STDERR "No datafiles!\n"
   }
   for my $datafile (sort keys %$known_datafiles) {
      btq_at_datafile($task_queue,$modelfile,$datafile,$args,"(" . basename($modelfile) . " + " . basename($datafile) . ")")
   }
}

sub btq_at_datafile {
   my($task_queue,$modelfile,$datafile,$args,$loc) = @_;
   my $known_datafiles    = $$args{datafiles};
   my $applicable_configs = $$known_datafiles{$datafile};
   die "'applicable_configs' should be an array" unless (ref $applicable_configs eq ref []);
   if (@$applicable_configs == 0) {
      print STDERR "No applicable configs for $loc!\n"
   }
   for my $config_name (sort @$applicable_configs) {
      my $known_configs = $$args{configs};
      my $config        = $$known_configs{$config_name};
      die "No config for '$config_name'" unless defined $config;
      btq_at_config($task_queue,$modelfile,$datafile,$config,$args,"$loc/$config_name")
   }
}

sub btq_at_config {
   my($task_queue,$modelfile,$datafile,$config,$args,$loc) = @_;
   # Just need to look at the strategies for the configuration, as all the information about
   # defaults and annotations has been "flattened" into each strategy during YAML interpretation.
   my $strategies = $$config{strategies};
   die "'strategies' should be a hash" unless (ref $strategies eq ref {});
   if (%$strategies == 0) {
      print STDERR "No strategies for $loc!\n"
   }
   for my $strategy_name (sort keys %$strategies) {
      my $strategy = $$strategies{$strategy_name};
      btq_at_strategy($task_queue,$modelfile,$datafile,$strategy,$args,"$loc/$strategy_name")
   }
}

sub btq_at_strategy {
   my($task_queue,$modelfile,$datafile,$strategy,$args,$loc) = @_;
   my $rounds = $$strategy{rounds};
   if ($rounds <= 0) {
      print STDERR "No rounds for $loc!\n"
   }
   for (my $round = 1; $round <= $rounds; $round++) { # 1-based for once
      my $base_df = basename($datafile);
      my $base_mf = basename($modelfile);
      if ($base_df =~ /^(.+)\.dzn$/) { $base_df = $1 }
      if ($base_mf =~ /^(.+)\.mzn$/) { $base_mf = $1 }
      push @$task_queue,
         {  modelfile     => $modelfile
           ,datafile      => $datafile
           ,resultfile    => $$args{resultfile}
           ,base_mf       => $base_mf
           ,base_df       => $base_df
           ,round         => $round
           ,rounds        => $rounds
           ,ann_name      => $$strategy{ann_name}
           ,annotation    => $$strategy{annotation}
           ,limit_s       => $$strategy{limit_s}
           ,obj_name      => $$strategy{obj_name} }
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


