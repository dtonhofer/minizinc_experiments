#!/usr/bin/perl

###############################################################################
# Interprete the "alias" blocks in the configuration YAML.
#
# The homepage for this script with additional explanation is at
# https://github.com/dtonhofer/minizinc_experiments/tree/main/stats_collection
###############################################################################

package Mzn::ReadYamlAliases;

use strict;
use warnings;
use utf8;  # Meaning "This lexical scope (i.e. file) contains utf8"

use Clone qw(clone);
use Mzn::Helpers qw(is_hashref is_arrayref is_nonref);

use Exporter qw(import);
our @EXPORT_OK = qw(interprete_aliases_collection
                    ALIASES_INDIC
                    VSS_INDIC
                    DSS_INDIC);

our $ALIASES_INDIC = 'aliases';
our $VSS_INDIC     = 'variable_selection_strategy';
our $DSS_INDIC     = 'domain_splitting_strategy';

sub interprete_aliases_collection {
   my($yml,$aliases_vss,$aliases_dss) = @_;
   my $error_count = 0;
   my $new_aliases_vss;
   my $new_aliases_dss;
   if (exists $$yml{$ALIASES_INDIC}) {
      my $subyml = $$yml{$ALIASES_INDIC};
      if (is_hashref($subyml)) {
         my $e;
         ($e,$new_aliases_vss,$new_aliases_dss) = interprete_aliases_collection_that_exists($subyml,$aliases_vss,$aliases_dss,$ALIASES_INDIC);
         $error_count += $e
      }
      else {
         print STDERR "Whatever is stored under '$ALIASES_INDIC' is not a hash.\n";
         $error_count++
      }
   }
   else {
      print STDERR "Nothing to do as there are no aliases under '$ALIASES_INDIC'\n";
      # Nothing to do as there are no aliases
   }
   return ($error_count,$new_aliases_vss,$new_aliases_dss);
}

sub interprete_aliases_collection_that_exists {
   my($yml,$aliases_vss,$aliases_dss,$indic_here) = @_;
   my $error_count = 0;
   # These change the "aliases_vss" and "aliases_dss" in-place
   my ($e1,$new_aliases_vss) = interprete_aliases($yml,$VSS_INDIC,$aliases_vss,"VSS",$indic_here);
   my ($e2,$new_aliases_dss) = interprete_aliases($yml,$DSS_INDIC,$aliases_dss,"DSS",$indic_here);
   $error_count = $error_count + $e1 + $e2;
   return ($error_count,$new_aliases_vss,$new_aliases_dss)
}

sub interprete_aliases {
   my($yml,$indic,$aliases,$type,$indic_here) = @_;
   my $error_count = 0;
   my $new_aliases;
   if (exists $$yml{$indic}) {
      my $subyml = $$yml{$indic};
      my $loc = "$indic_here/$indic";
      if (is_hashref($subyml)) {
         my $e;
         ($e,$new_aliases) = interprete_aliases_that_exist($subyml,$aliases,$type,$loc);
         $error_count += $e
      }
      else {
         print STDERR "Whatever is stored under '$loc' is not a hash.\n";
         $error_count++
      }
   }
   else {
      # Nothing to do as there are no such aliases
   }
   return ($error_count,$new_aliases)
}

sub interprete_aliases_that_exist {
   my($yml,$aliases,$type,$indic_here) = @_;
   my $error_count = 0;
   my $new_aliases = clone($aliases); # CLONE! work on a temporary map before doing in-place replacement
   for my $alias_key (keys %$yml) {
      my $loc = "$indic_here/$alias_key";
      my $alias_val = $$yml{$alias_key};
      if (is_nonref($alias_val)) {
         if (!exists $$new_aliases{$alias_val}) {
            # There should be an entry for $alias_val in $aliases because
            # because all the allowed (non-alias) keywords are (presumably)
            # already in there, mapping to themselves. Emit a warning!
            print STDERR "Warning: In the configuration, the $type aliases contain the mapping '$alias_key' => '$alias_val', but '$alias_val' is unknown.\n";
         }
         if (exists $$new_aliases{$alias_key}) {
            # Really only possible if the program has set one up as it can come from the YAML
            # which disallows multiple same keys
            print STDERR "An entry for '$alias_key' already exists in the $type aliases.\n";
            print STDERR "Existing value : '$$new_aliases{$alias_key}'.\n";
            print STDERR "New value      : '$alias_val'.\n";
         }
         $$new_aliases{$alias_key} = $alias_val;
      }
      else {
         print STDERR "Whatever is stored under '$loc' is not a string.\n";
         $error_count++
      }
   }
   if ($error_count == 0) {
      ($$aliases{$_} = $$new_aliases{$_}) for keys %$new_aliases; # copy over
   }
   return ($error_count,$new_aliases)
}

