#!/usr/bin/perl

###############################################################################
# This module deals with setting up the "task queue", where each task describes
# a MiniZinc process to be executed.
#
# The homepage for this script with additional explanation is at
# https://github.com/dtonhofer/minizinc_experiments/tree/main/stats_collection
###############################################################################

package Mzn::TaskQueue;

use strict;
use warnings;
use utf8;  # Meaning "This lexical scope (i.e. file) contains utf8"

use YAML::Tiny;     # install with "dnf install perl-YAML-Tiny"
use Data::Dumper;   # install with "dnf install perl-Data-Dumper"
use File::Basename;
use Fcntl qw(:flock SEEK_END);
use Mzn::Helpers qw(is_hashref is_arrayref is_nonref);

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
   die "'known modelfiles' should be an array" unless is_arrayref($known_modelfiles);
   die "'known modelfiles' should be nonempty" unless (@$known_modelfiles > 0);
   for my $modelfile (sort @$known_modelfiles) {
      btq_at_modelfile($task_queue,$modelfile,$args);
   }
   return $task_queue
}

sub btq_at_modelfile {
   my($task_queue,$modelfile,$args) = @_;
   my $known_datafiles = $$args{datafiles};
   die "'known datafiles' should be a hash" unless is_hashref($known_datafiles);
   die "'known datafiles' should be nonempty" unless (%$known_datafiles > 0);
   for my $datafile (sort keys %$known_datafiles) {
      btq_at_datafile($task_queue,$modelfile,$datafile,$args,"(" . basename($modelfile) . " + " . basename($datafile) . ")")
   }
}

sub btq_at_datafile {
   my($task_queue,$modelfile,$datafile,$args,$loc) = @_;
   my $known_datafiles = $$args{datafiles};
   my $applicable_configs = $$known_datafiles{$datafile};
   die "'applicable configs' of '$datafile' should be an array" unless is_arrayref($applicable_configs);
   die "'applicable configs' of '$datafile' should be nonempty" unless (@$applicable_configs > 0);
   my $known_configs = $$args{configs};
   die "'known configs' should be a hash" unless is_hashref($known_configs);
   die "'known configs' should be nonempty" unless (%$known_configs > 0);
   for my $config_name (sort @$applicable_configs) {
      my $config = $$known_configs{$config_name};
      die "No config with name '$config_name' exists" unless defined $config;
      die "Config with name '$config_name' should be a hash" unless is_hashref($config);
      die "Config with name '$config_name' should be nonempty" unless (%$config > 0);
      btq_at_config($task_queue,$modelfile,$datafile,$config_name,$config,$args,"$loc/$config_name")
   }
}

sub btq_at_config {
   my($task_queue,$modelfile,$datafile,$config_name,$config,$args,$loc) = @_;
   # We just need to look at the "applications" for the configuration, as all the information about
   # defaults and annotations has been "flattened" into each application during YAML interpretation.
   my $applications = $$config{applications};
   die "No applications found in '$loc'" unless defined $applications;
   die "Applications in '$loc' should be a hash" unless is_hashref($applications);
   die "Applications in '$loc' should be nonempty" unless (%$applications > 0);
   for my $application_name (sort keys %$applications) {
      my $application = $$applications{$application_name};
      btq_at_application($task_queue,$modelfile,$datafile,$config_name,$application_name,$application,$args,"$loc/$application_name")
   }
}

sub btq_at_application {
   my($task_queue,$modelfile,$datafile,$config_name,$application_name,$application,$args,$loc) = @_;
   my $rounds = $$application{rounds};
   die "Rounds must be defined in '$loc'" unless defined $rounds;
   if ($rounds <= 0) {
      print STDERR "No rounds for '$loc' as rounds = $rounds!\n"
   }
   for (my $round = 1; $round <= $rounds; $round++) { # 1-based for once
      my $modelfile_base  = basename($modelfile);
      my $datafile_base   = basename($datafile);
      if ($modelfile_base =~ /^(.+)\.mzn$/) { $modelfile_base = $1 }
      if ($datafile_base  =~ /^(.+)\.dzn$/) { $datafile_base = $1 }
      my $flattened = {
          modelfile        => $modelfile
         ,datafile         => $datafile
         ,config_name      => $config_name
         ,application_name => $application_name
         ,resultfile       => $$args{resultfile}
         ,modelfile_base   => $modelfile_base
         ,datafile_base    => $datafile_base
         ,round            => $round
         ,rounds           => $rounds
         ,limit_s          => $$application{limit_s}
         ,objval_name      => $$application{objval_name} };
      if (exists $$application{annotations}) {
         my $annotations = $$application{annotations};
         for my $type ("search", "restart") {
            if (exists $$annotations{$type}) {
               $$flattened{$type} = $$annotations{$type}
            }
         }
      }
      push @$task_queue, $flattened;
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


