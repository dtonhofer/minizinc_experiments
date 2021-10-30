#!/usr/bin/perl

###############################################################################
# Toplevel routine which reads the YAML configuratoin file with YAML::Tiny
# and then calls routines exported from other modules to interprete it.
#
# The homepage for this script with additional explanation is at
# https://github.com/dtonhofer/minizinc_experiments/tree/main/stats_collection
###############################################################################

package Mzn::ReadYaml;

use strict;
use warnings;
use utf8;  # Meaning "This lexical scope (i.e. file) contains utf8"

use YAML::Tiny;     # install with "dnf install perl-YAML-Tiny"
use Data::Dumper;   # install with "dnf install perl-Data-Dumper"
use File::Basename;
use List::Util qw(min max);
use Clone qw(clone);

use Exporter qw(import);
our @EXPORT_OK = qw(read_and_interprete_yaml);

use Mzn::Helpers qw(is_hashref is_arrayref is_nonref);
use Mzn::ReadYamlAliases qw(interprete_aliases_collection);
use Mzn::ReadYamlModelfiles qw(interprete_modelfiles);
use Mzn::ReadYamlDatafiles qw(interprete_datafiles);
use Mzn::ReadYamlConfigCollection qw(interprete_config_collection);
use Mzn::ReadYamlDefaults qw(interprete_and_merge_defaults);

sub read_and_interprete_yaml {
   my($args) = @_;
   #
   # Read file, exiting immediately on error
   #
   my $yml = read_yaml_configfile($$args{configfile});
   if ($$args{debug_config}) {
      print STDERR "Reading YAML file yields the following:\n\n";
      print STDERR Data::Dumper->new([$yml])->Sortkeys(1)->Dump, "\n";
   }
   #
   # Analyze what was read. A nonzero error count is interpreted as "fatal error"
   #
   my $error_count = interprete_yml($yml,$args);
   if ($$args{debug_config}) {
      print STDERR "\n";
      print STDERR "Variable selection strategy aliases:\n\n";
      print STDERR Data::Dumper->new([$$args{aliases_vss}])->Sortkeys(1)->Dump, "\n";
      print STDERR "Domain splitting strategy aliases:\n\n";
      print STDERR Data::Dumper->new([$$args{aliases_dss}])->Sortkeys(1)->Dump, "\n";
      print STDERR "Configurations:\n\n";
      print STDERR Data::Dumper->new([$$args{configs}])->Sortkeys(1)->Dump, "\n";
      print STDERR "Data files:\n\n";
      print STDERR Data::Dumper->new([$$args{datafiles}])->Sortkeys(1)->Dump, "\n";
      print STDERR "Model files:\n\n";
      print STDERR Data::Dumper->new([$$args{modelfiles}])->Sortkeys(1)->Dump, "\n";
   }
   if ($error_count > 0) {
      print STDERR "Errors occurred while interpreting the YAML document -- exiting\n";
      exit 1
   }
}

sub read_yaml_configfile {
   my($configfile) = @_;
   if (! -f $configfile) {
      print STDERR "The config file '$configfile' does not exist or is not a file -- exiting\n";
      exit 1
   }
   my $all_yaml = YAML::Tiny->read($configfile); # the file is supposed encoded in UTF-8
   my $yml      = $all_yaml->[0];                # we suppose content is in document 0
   if (!$yml) {
      print STDERR "There is nothing in YAML document 0. Is the config file empty? -- exiting\n";
      exit 1
   }
   return $yml;
}

# YAML interpretation. $yml is the structure generated by YAML::Tiny->read(), a
# structure of hashes, arrays and scalars. $args is a hash that will be completed
# in-place by interpreting what is in $yml. This procedure just returns the
# error count.

sub interprete_yml {
   my($yml,$args) = @_;
   my $error_count = 0;
   {
      my $existing_alz_vss = $$args{aliases_vss};
      my $existing_alz_dss = $$args{aliases_dss};
      my ($e,$new_alz_vss,$new_alz_dss) = interprete_aliases_collection($yml,$existing_alz_vss,$existing_alz_dss);
      $error_count += $e;
      if ($e==0) {
         $$args{aliases_vss} = $new_alz_vss;
         $$args{aliases_dss} = $new_alz_dss
      }
   }
   {
      my $superdefaults = {
          rounds      => 1      # generally 1 round, assuming search is deterministic
         ,limit_s     => 0      # no limit!
         ,objval_name => "obj"  # generally the "objective value" variable name is "obj"
      };
      my ($e,$defaults) = interprete_and_merge_defaults($yml,$args,$superdefaults);
      $error_count += $e;
      if ($e==0) {
         $$args{defaults} = $defaults;
      }
   }
   {
      my ($e,$config_coll) = interprete_config_collection($yml,$args);
      $error_count += $e;
      if ($e==0) {
         if (%$config_coll > 0) {
            $$args{configs} = $config_coll
         } else {
            print STDERR "No valid configurations found -- this is a fatal error!\n";
            $error_count++
         }
      }
   }
   {
      my ($e,$datafiles) = interprete_datafiles($yml,$args);
      $error_count += $e;
      if ($e==0) {
         if (%$datafiles > 0) {
            $$args{datafiles} = $datafiles
         } else {
            print STDERR "No datafiles have been retained -- that is a fatal error!\n";
            $error_count++
         }
      }
   }
   {
      my ($e,$modelfiles) = interprete_modelfiles($yml,$args);
      $error_count += $e;
      if ($e==0) {
         if (@$modelfiles > 0) {
            $$args{modelfiles} = $modelfiles
         } else {
            print STDERR "No modelfiles have been retained -- that is a fatal error!\n";
            $error_count++
         }
      }
   }
   return $error_count
}

