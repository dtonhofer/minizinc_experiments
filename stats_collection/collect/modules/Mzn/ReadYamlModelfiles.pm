#!/usr/bin/perl

###############################################################################
# Interprete the "modelfiles" block in the YAML configuration file.
#
# The homepage for this script with additional explanation is at
# https://github.com/dtonhofer/minizinc_experiments/tree/main/stats_collection
###############################################################################

package Mzn::ReadYamlModelfiles;

use strict;
use warnings;
use utf8;  # Meaning "This lexical scope (i.e. file) contains utf8"

use Clone qw(clone);
use File::Basename;
use Mzn::Helpers qw(is_hashref is_arrayref is_nonref);

use Exporter qw(import);
our @EXPORT_OK = qw(interprete_modelfiles MODELFILES_INDIC);

our $MODELFILES_INDIC = 'modelfiles';

sub interprete_modelfiles {
   my($yml,$args) = @_;
   my $error_count = 0;
   my $modelfiles = {};
   if (exists $$yml{$MODELFILES_INDIC}) {
      my $subyml = $$yml{$MODELFILES_INDIC}; # for now, an array
      if (is_arrayref($subyml)) {
         my $e;
         ($e,$modelfiles) = interprete_modelfiles_that_exist($subyml,$args,$MODELFILES_INDIC);
         # $error_count += $e; errors are not fatal
      }
      else {
         print STDERR "Whatever is stored under '$MODELFILES_INDIC' is not an array.\n";
         $error_count++
      }
   }
   else {
      print STDERR "Warning: there is no information about modelfiles. It should be under '$MODELFILES_INDIC'!\n";
      $error_count++
   }
   return ($error_count,$modelfiles); # if "modelfiles" is empty, the caller will protest
}

sub interprete_modelfiles_that_exist {
   my($array,$args,$indic_here) = @_;
   my $error_count = 0;
   my $modelfiles = [];
   for my $filename (@$array) {
      my $abs_filename;
      if (File::Spec->file_name_is_absolute($filename)) {
         $abs_filename = $filename;
      }
      else {
         $abs_filename = File::Spec->catfile($$args{model_dir},$filename)
      }
      if (-f $abs_filename) {
         push @$modelfiles, $abs_filename;
      }
      else {
         print STDERR "The modelfile '$abs_filename' does not exist -- skipping that model file!\n";
         $error_count++ # count up but the caller will disregard this
      }
   }
   return ($error_count,$modelfiles)
}

