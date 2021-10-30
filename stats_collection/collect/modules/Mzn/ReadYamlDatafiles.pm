#!/usr/bin/perl

###############################################################################
# Interprete the "datafiles" block in the YAML config file.
#
# The homepage for this script with additional explanation is at
# https://github.com/dtonhofer/minizinc_experiments/tree/main/stats_collection
###############################################################################

package Mzn::ReadYamlDatafiles;

use strict;
use warnings;
use utf8;  # Meaning "This lexical scope (i.e. file) contains utf8"

use Clone qw(clone);
use File::Basename;
use Mzn::Helpers qw(is_hashref is_arrayref is_nonref);

use Exporter qw(import);
our @EXPORT_OK = qw(interprete_datafiles  DATAFILES_INDIC);

our $DATAFILES_INDIC = 'datafiles';

sub interprete_datafiles {
   my($yml,$args) = @_;
   my $error_count = 0;
   my $datafiles = {};
   if (exists $$yml{$DATAFILES_INDIC}) {
      my $subyml = $$yml{$DATAFILES_INDIC};
      if (is_hashref($subyml)) {
         my $e;
         ($e,$datafiles) = interprete_datafiles_that_exist($subyml,$args,$DATAFILES_INDIC);
         # $error_count += $e not a fatal error
      }
      else {
         print STDERR "Whatever is stored under '$DATAFILES_INDIC' is not a hash.\n";
         $error_count++
      }
   }
   else {
      print STDERR "Warning: there is no information about datafiles. It should be under '$DATAFILES_INDIC'!\n";
      # TODO: If a datafile is not actually needed for the model, one would need to define a dummy.
      # One way to avoid that would be to have a "nodatafiles" entry mapping to the appropriate config.
      $error_count++;
   }
   return ($error_count,$datafiles); # if "datafiles" is empty, the caller will protest
}

sub interprete_datafiles_that_exist {
   my($yml,$args,$indic_here) = @_;
   my $error_count = 0;
   my $datafiles = {};
   for my $filename (keys %$yml) {
      my $loc = "$indic_here/$filename";
      my ($abs_filename,$base_filename) = absolutize_filename($filename,$$args{data_dir});
      if (-f $abs_filename) {
         my $known_configs = $$args{configs};
         my($e,$confirmed) = interprete_configs_associated_to_datafile($yml,$filename,$known_configs,$loc);
         # disregard error count, check that at least 1 config has been retained
         if (@$confirmed == 0) {
            print STDERR "No configs retained for data file '$abs_filename' at '$loc'. Need at least one -- skipping this data file!\n";
            # disregard $error_count++
         }
         else {
            $$datafiles{$abs_filename} = $confirmed # 1 or more configs in an array
         }
      }
      else {
         print STDERR "The data file '$abs_filename' at '$loc' does not seem to exist -- skipping this data file!\n";
         # $error_count += $e; Do not count as fatal error
      }
   }
   return ($error_count,$datafiles);
}

sub absolutize_filename {
   my($filename,$data_dir) = @_;
   my $abs_filename;
   my $base_filename;
   if (File::Spec->file_name_is_absolute($filename)) {
      # only happens if the hash key (i.e. $filename) looks like an absolute filename
      $abs_filename = $filename
   }
   else {
      $abs_filename = File::Spec->catfile($data_dir,$filename)
   }
   $base_filename = basename($abs_filename);
   return ($abs_filename,$base_filename);
}

sub interprete_configs_associated_to_datafile {
   my($yml,$filename,$known_configs,$indic_here) = @_;
   my $error_count = 0;
   my $confirmed = [];
   #
   # Various things may have been stored at "filename"
   #
   my $as_passed;
   {
      my $value = $$yml{$filename};
      if (is_nonref($value)) {
         $as_passed = [$value]
      }
      elsif (is_arrayref($value)) {
         $as_passed = $value
      }
      else {
         print STDERR "Neither a string nor an array at '$indic_here' -- disregarding this entry!\n";
         $error_count++
      }
   }
   #
   # Cross-check config names if no error yet
   #
   if ($error_count == 0) {
      my $e;
      ($e,$confirmed) = traverse_array_of_configs($as_passed,$known_configs,$indic_here);
      $error_count += $e
   }
   return ($error_count,$confirmed)
}

sub traverse_array_of_configs {
   my($array,$known_configs,$indic_here) = @_;
   my $confirmed = [];
   my $error_count = 0;
   for my $maybe_config_name (@$array) {
      if (exists $$known_configs{$maybe_config_name}) {
         push @$confirmed, $maybe_config_name;
      }
      else {
         print STDERR "There is no configuration '$maybe_config_name' as requested at '$indic_here' -- disregarding this entry!\n";
         $error_count++
      }
   }
   return ($error_count,$confirmed) # returns number of discard and retained entries
}

