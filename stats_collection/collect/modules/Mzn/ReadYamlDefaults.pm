#!/usr/bin/perl

###############################################################################
# Interprete the "defaults" block in the YAML config file. This is used to
# interprete the toplevel "defaults" block and the "configuration" specific
# defaults block overriding the toplevel "defaults" block.
#
# The homepage for this script with additional explanation is at
# https://github.com/dtonhofer/minizinc_experiments/tree/main/stats_collection
###############################################################################

package Mzn::ReadYamlDefaults;

use strict;
use warnings;
use utf8;  # Meaning "This lexical scope (i.e. file) contains utf8"

use List::Util qw(min max);
use Mzn::Helpers qw(is_hashref is_nonref clone_and_merge);

use Exporter qw(import);
our @EXPORT_OK = qw(interprete_and_merge_defaults
                    parse_limit_seconds
                    parse_rounds
                    rounds_indic
                    limit_seconds_indic);

my $DEFAULTS_INDIC           = 'defaults';
my $ROUNDS_INDIC             = 'rounds';
my $LIMIT_SECONDS_INDIC      = 'limit_s';
my $OBJVAL_NAME_INDIC        = 'objval_name';
my $SEARCH_ANNOTATION_INDIC  = 'search';
my $RESTART_ANNOTATION_INDIC = 'restart';
my $ANN_NAME_INDIC           = 'name';
my $ANN_TEMPLATE_INDIC       = 'template';

# Stand-ins for constants

sub rounds_indic() {
   return $ROUNDS_INDIC
}

sub limit_seconds_indic() {
   return $LIMIT_SECONDS_INDIC
}

# Existing "defaults" are passed, new defaults are extracted from "yml"
# then the existing "defaults" are cloned and overridden by those
# "new defaults". This gives "overridden defaults" which are returned.

sub interprete_and_merge_defaults {
   my($yml,$args,$defaults) = @_;
   my $error_count = 0;
   my $new_defaults = {};
   my $overridden_defaults = {};
   if (exists $$yml{$DEFAULTS_INDIC}) {
      my $subyml = $$yml{$DEFAULTS_INDIC};
      if (is_hashref($subyml)) {
         my $e;
         ($e,$new_defaults) = interprete_defaults_that_exists($subyml,$args,$DEFAULTS_INDIC);
         $error_count += $e
      }
      else {
         print STDERR "Whatever is stored under '$DEFAULTS_INDIC' is not a hash!\n";
         $error_count++
      }
   }
   else {
      print STDERR "Warning: there is no '$DEFAULTS_INDIC' structure!\n";
   }
   if ($error_count == 0) {
      $overridden_defaults = clone_and_merge($defaults,$new_defaults)
   }
   return ($error_count,$overridden_defaults)
}

sub interprete_defaults_that_exists {
   my($yml,$args,$indic_here) = @_;
   my $new_defaults = {};
   my $error_count = 0;
   if (exists $$yml{$ROUNDS_INDIC}) {
      my $subyml = $$yml{$ROUNDS_INDIC};
      my $loc    = "$indic_here/$ROUNDS_INDIC";
      my $x = parse_rounds($subyml,$loc); # returns undef if bad
      if (defined $x) {
         $$new_defaults{rounds} = $x
      }
   }
   if (exists $$yml{$LIMIT_SECONDS_INDIC}) {
      my $subyml = $$yml{$LIMIT_SECONDS_INDIC};
      my $loc    = "$indic_here/$LIMIT_SECONDS_INDIC";
      my $x = parse_limit_seconds($subyml,$loc); # returns undef if bad
      if (defined $x) {
         $$new_defaults{limit_s} = $x
      }
   }
   if (exists $$yml{$OBJVAL_NAME_INDIC}) {
      my $subyml = $$yml{$OBJVAL_NAME_INDIC};
      my $loc    = "$indic_here/$OBJVAL_NAME_INDIC";
      my $x = parse_name($subyml,$loc); # returns undef if bad
      if (defined $x) {
         $$new_defaults{objval_name} = $x
      }
   }
   # "search annotation" block, possibly missing or incomplete
   {
      my ($e,$annot) = interprete_annotation($yml,$SEARCH_ANNOTATION_INDIC,$indic_here);
      $error_count += $e;
      if ($e == 0 && $annot) {
         # if it has been registered, it is not fully empty
         $$new_defaults{search} = $annot;
      }
   }
   # "restart annotation" block, possibly missing or incomplete
   {
      my ($e,$annot) = interprete_annotation($yml,$RESTART_ANNOTATION_INDIC,$indic_here);
      $error_count += $e;
      if ($e == 0 && $annot) {
         # if it has been registered, it is not fully empty
         $$new_defaults{restart} = $annot;
      }
   }
   return ($error_count,$new_defaults)
}

# This is called for "search" and "restart" annotations.
# On return with $error_count = 0, the "$annot" may be fully
# undef if missing, or one or the other of its subvalues may
# be missing.
# "$type" takes on one of the values "search" or "restart".

sub interprete_annotation {
   my($yml,$type,$indic_here) = @_;
   my $error_count = 0;
   my $annot;
   my $loc = "$indic_here/$type";
   if (exists $$yml{$type}) {
      $annot = {};
      my $subyml = $$yml{$type};
      if (is_hashref($subyml)) {
         my ($e,$name,$template) = interprete_annotation_that_exists($subyml,$loc); # either of name or template may be undef
         $error_count += $e;
         if ($e == 0) {
            if ($name)     { $$annot{name}     = $name     }
            if ($template) { $$annot{template} = $template }
         }
         die "Annotation is fully empty" unless %$annot > 0;
      }
      else {
         print STDERR "Whatever is stored under '$loc' is not a hash.\n";
         $error_count++;
      }
   }
   else {
      # it's missing? That's ok! $annot will stay the empty hash
   }
   return ($error_count,$annot);
}

# This is called for "search" and "restart" annotations.
# The "$yml" is the block:
#
#   search:
#      name      : my_search_annotation
#      template  : "int_search(s, $vss, $dss)"
#   restart:
#      name     : my_restart_annotation
#      template : "restart_geometric($base, $scale)"

sub interprete_annotation_that_exists {
   my($yml,$indic_here) = @_;
   my $error_count = 0;
   my ($e1,$name)     = read_annotation_name($yml,$indic_here);     # may be missing, then $name = undef
   my ($e2,$template) = read_annotation_template($yml,$indic_here); # may be missing, then $template = undef
   $error_count = $error_count + $e1 + $e2;
   return ($error_count,$name,$template);
}

sub read_annotation_name {
   my($yml,$indic_here) = @_;
   my $error_count = 0;
   my $name;
   my $loc = "$indic_here/$ANN_NAME_INDIC";
   if (exists $$yml{$ANN_NAME_INDIC}) {
      $name = $$yml{$ANN_NAME_INDIC};
      if (is_nonref($name) && $name =~ /^[A-Za-z][A-Za-z0-9_]*$/) {
         # Success, it is a name
      }
      else {
         print STDERR "Value '$name' found at '$loc' does not look like an identifier!\n";
         $error_count++
      }
   }
   else {
      # Missing name? That's ok for now (but may turn out to be a problem later)
   }
   return ($error_count,$name)
}

# Just retrieve the annotation template.
# We leave it to MiniZinc to check the syntax, which can be complex.
# (Besides, we have to replace any placeholders that are in the template)
# The template may be missing, in which case the value will be undef.

sub read_annotation_template {
   my($yml,$indic_here) = @_;
   my $error_count = 0;
   my $template;
   my $loc = "$indic_here/$ANN_TEMPLATE_INDIC";
   if (exists $$yml{$ANN_TEMPLATE_INDIC}) {
      $template = $$yml{$ANN_TEMPLATE_INDIC};
      # Success, no further checks;
   }
   else {
      # Missing template? That's ok for now (but may turn out to be a problem later)
   }
   return ($error_count,$template)
}

# Strictly positive integer: return integer ("set rounds")
# Anything else: write a warning and return undef (the entry will be ignored and
# not override anything)

sub parse_rounds {
   my($value,$indic_here) = @_;
   my $x;
   if (is_nonref($value) && $value =~ /^[0-9]+$/) {
      $x = $value * 1
   }
   if (defined($x) && $x > 0) {
      return $x
   }
   else {
      print STDERR "Value '$value' at '$indic_here' doesn't look like a strictly positive integer. Ignored!\n";
      return undef
   }
}

# Positive integer: return integer ("set limit")
# Negative or 0 integer: return 0 ("unset limit")
# Anything else: write a warning and return undef (the entry will be ignored and
# not override anything)

sub parse_limit_seconds {
   my($value,$indic_here) = @_;
   if (is_nonref($value) && $value =~ /^\-?[0-9]+$/) {
      return max(0,$value * 1)
   }
   else {
      print STDERR "Value '$value' at '$indic_here' doesn't look like an integer. Ignored!\n";
      return undef
   }
}

# Check identifier according to
# https://www.minizinc.org/doc-2.5.5/en/spec.html?highlight=variable%20name#spec-identifiers

sub parse_name {
   my($value,$indic_here) = @_;
   if (is_nonref($value) && $value =~ /^[A-Za-z][A-Za-z0-9_]*$/) {
      return $value
   }
   else {
      print STDERR "Value '$value' at '$indic_here' doesn't look like a valid variable name. Ignored!\n";
      return undef
   }
}

