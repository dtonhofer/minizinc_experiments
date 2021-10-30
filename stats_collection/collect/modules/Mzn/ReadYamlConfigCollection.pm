#!/usr/bin/perl

###############################################################################
# Interprete the "configuration" block or blocks that can be found in the
# YAML config file.
#
# The homepage for this script with additional explanation is at
# https://github.com/dtonhofer/minizinc_experiments/tree/main/stats_collection
###############################################################################

package Mzn::ReadYamlConfigCollection;

use strict;
use warnings;
use utf8;  # Meaning "This lexical scope (i.e. file) contains utf8"

use Clone qw(clone);
use File::Basename;
use Data::Dumper;
use Mzn::Helpers qw(is_hashref is_arrayref is_nonref);
use Mzn::ReadYamlDefaults qw(interprete_and_merge_defaults
                             parse_limit_seconds
                             parse_rounds
                             limit_seconds_indic
                             rounds_indic);

use Exporter qw(import);
our @EXPORT_OK = qw(interprete_config_collection);

sub interprete_config_collection {
   my($yml,$args) = @_; # the defaults are in the args
   my $error_count = 0;
   my $config_coll = {}; # maps config name to a hash describing the config
   die unless is_hashref($yml);
   for my $key (keys %$yml) {
      if ($key =~ /^config_/) {
         my $loc    = $key;
         my $subyml = $$yml{$key};
         my($e,$config) = interprete_config_that_exists($subyml,$args,$loc,$$args{defaults});
         $error_count += $e;
         if ($e == 0) {
            $$config_coll{$key} = $config
         }
      }
   }
   return ($error_count,$config_coll)
}

sub interprete_config_that_exists {
   my($yml,$args,$indic_here,$defaults) = @_;
   my $error_count = 0;
   my $config = {};
   my $new_defaults;
   # There *may* be new defaults inside the config which have to be merged with the
   # defaults that have already been obtained
   {
      my $e;
      ($e,$new_defaults) = interprete_and_merge_defaults($yml,$args,$defaults);
      $error_count += $e;
      # print STDERR "After merging the defaults in '$indic_here' with error_count = $e\n";
      # print STDERR Data::Dumper->new([$new_defaults])->Sortkeys(1)->Dump, "\n"
   }
   # Make sure we have valid search and restart annotations at this point
   $error_count += check_that_annotations_are_complete($new_defaults,$indic_here);
   if ($error_count == 0) {
      my($e,$applications) = interprete_applications_inside_config($yml,$args,$new_defaults,$indic_here);
      $error_count += $e;
      $$config{applications} = $applications;
      # print STDERR "After collecting applications in '$indic_here' with error_count = $e\n";
      # print STDERR Data::Dumper->new([$config])->Sortkeys(1)->Dump, "\n"
   }
   return ($error_count,$config);
}

sub check_that_annotations_are_complete {
   my($defaults,$indic_here) = @_;
   my $error_count = 0;
   for my $name ("search","restart") {
      if (exists $$defaults{$name}) {
         my $subyml = $$defaults{$name};
         if (exists $$subyml{name} && exists $$subyml{template}) {
            # good
         }
         else {
            $error_count++;
            print STDERR "Incomplete '$name' annotation block at '$indic_here' after reading all defaults!\n"
         }
      }
   }
   if ($error_count > 0) {
      print STDERR Data::Dumper->new([$defaults])->Sortkeys(1)->Dump, "\n"
   }
   return $error_count
}

# A "config" is a hash of "application" entries, which are hashes holding all
# the information needed for running a MiniZinc process

sub interprete_applications_inside_config {
   my($yml,$args,$defaults,$indic_here) = @_;
   my $error_count = 0;
   my $applications = {};
   for my $key (sort keys %$yml) {
      if ($key =~ /^application_/) {
         my $loc    = "$indic_here/$key";
         my $subyml = $$yml{$key};
         my($e,$application) = interprete_application_that_exists($subyml,$args,$defaults,$loc);
         $error_count += $e;
         if ($e == 0) {
            $$applications{$key} = $application
         }
      }
      else {
         # Skip over anything that has a key that doesn't start with the string "application_"
         # print STDERR "Skipping key '$key' at '$indic_here': not an application.\n";
      }
   }
   return ($error_count,$applications)
}

sub interprete_application_that_exists {
   my($yml,$args,$defaults,$indic_here) = @_;
   my $error_count = 0;
   my $application = {};
   $error_count += merge_annotations_into_application($yml,$application,$args,$defaults,$indic_here);
   $error_count += merge_rounds_and_limits_into_application($yml,$application,$defaults,$indic_here);
   $error_count += merge_objval_name_into_application($application,$defaults,$indic_here);
   return ($error_count,$application)
}

sub merge_annotations_into_application {
   my($yml,$application,$args,$defaults,$indic_here) = @_;
   my ($error_count,$resolved_templates) = replace_placeholders_in_annotation_templates($yml,$args,$defaults,$indic_here);
   # print STDERR "Resolved templates of '$indic_here' with error_count = $e\n";
   # print STDERR "Defaults are ", Data::Dumper->new([$defaults])->Sortkeys(1)->Dump, "\n";
   # print STDERR "Result is ", Data::Dumper->new([$resolved_templates])->Sortkeys(1)->Dump, "\n";
   my $annotations = {};
   for my $type ("search","restart") {
      if (exists $$resolved_templates{$type}) {
         $$annotations{$type} =
         {
            name  => $$defaults{$type}{name},    # the name comes from the defaults
            value => $$resolved_templates{$type} # and the resolved template comes from the value returned above
         };
      }
   }
   $$application{annotations} = $annotations;
   return $error_count;
}

sub merge_rounds_and_limits_into_application {
   my($yml,$application,$defaults,$indic_here) = @_;
   my $error_count = 0;
   for my $key (rounds_indic(),limit_seconds_indic()) {
      if (exists $$defaults{$key}) {
         $$application{$key} = $$defaults{$key};
         if (exists $$yml{$key}) {
            my $v = $$yml{$key};
            my $x;
            if ($key eq rounds_indic()) {
               $x = parse_rounds($v,$indic_here); # returns undef if bad
            }
            elsif ($key eq limit_seconds_indic()) {
               $x = parse_limit_seconds($v,$indic_here); # returns undef if bad
            }
            else {
               die "Program error: $key"
            }
            if (defined $x) {
               $$application{$key} = $x
            }
            else {
               $error_count++
            }
         }
      }
      else {
         die "There is no default value for '$key' when inside '$indic_here', but there should be!";
      }
   }
   return $error_count
}

sub merge_objval_name_into_application {
   my($application,$defaults,$indic_here) = @_;
   my $key = "objval_name";
   if (exists $$defaults{$key}) {
      $$application{$key} = $$defaults{$key};
   }
   else {
      die "There is no default value for '$key' when inside '$indic_here', but there should be!";
   }
   return 0 # no errors
}

# $yml       : the application YAML block which is supposed to hold values for any placeholders
# $defaults  : completely up-to-date defaults from which we get the "search" and "restart" blocks

sub replace_placeholders_in_annotation_templates {
   my($yml,$args,$defaults,$indic_here) = @_;
   my $error_count = 0;
   my $resolved_templates = {};
   for my $type ("search","restart") {
      if (exists $$defaults{$type}) {
         my $annotation = $$defaults{$type};
         if (exists $$annotation{name} && exists $$annotation{template}) {
            my $template    = $$annotation{template};
            my $aliases_vss = $$args{aliases_vss};
            my $aliases_dss = $$args{aliases_dss};
            my ($e,$resolved) = replace_placeholder_recursively($yml,$type,$template,$template,$aliases_vss,$aliases_dss,$indic_here);
            $error_count += $e;
            if ($e == 0) {
               $$resolved_templates{$type} = $resolved
            }
         }
         else {
            print STDERR "Incomplete '$type' annotation block at '$indic_here' after reading all defaults!\n";
            print STDERR Data::Dumper->new([$defaults])->Sortkeys(1)->Dump, "\n";
            $error_count++
         }
      }
   }
   return ($error_count,$resolved_templates);
}

# Recursive replacement of placeholders in a tenplate.
#
# $yml         : is the "application" block holding (hopefully all) placeholder values
# $type        : is either "search" or "restart"; these are handled slightly differently
# $rest        : is the remaining template text that still has to be checked for placeholders
# $orig        : is the full original template text
# $aliases_dss : the known aliases for DSS
# $aliases_vss : the known aliases for VSS
# $indic_here  : the path to the "application" block

sub replace_placeholder_recursively {
   my($yml,$type,$rest,$orig,$aliases_vss,$aliases_dss,$indic_here) = @_;
   my $error_count = 0;
   my $fully_resolved;
   if ($rest =~ /^(.*?)(\$[A-Za-z][A-Za-z0-9_]*)(.*?)$/) {
      my $before      = $1;
      my $placeholder = $2;
      my $after       = $3;
      if ($type eq "search" && !($placeholder =~ /(vss|dss)/)) {
         print STDERR "The 'search' placeholder '$placeholder' in '$indic_here' does not contain the substring 'vss' or 'dss', but I need it to resolve through the VSS or DSS alias map.\n";
         $error_count++
      }
      else {
         my($e1,$after_resolved) = replace_placeholder_recursively($yml,$type,$after,$orig,$aliases_vss,$aliases_dss,$indic_here);
         my($e2,$resolved)       = resolve_placeholder($yml,$type,$placeholder,$aliases_vss,$aliases_dss,$indic_here);
         $error_count = $error_count + $e1 + $e2;
         if ($e1 + $e2 == 0) {
            $fully_resolved = $before . $resolved . $after_resolved;
         }
      }
   }
   elsif ($rest =~ /\$/) {
      print STDERR "Leftover '\$' that is not part of a placeholder found in template '$orig' at '$rest' in '$indic_here'\n";
      $error_count++
   }
   else {
      # nothing to resolve
      $fully_resolved = $rest
   }
   return ($error_count,$fully_resolved);
}

sub resolve_placeholder {
   my($yml,$type,$placeholder,$aliases_vss,$aliases_dss,$indic_here) = @_;
   my $error_count = 0;
   my $result;
   # get rid of the initial dollar
   $placeholder =~ /^\$(.+)$/ or die "Placeholder '$placeholder' does not match '\$X'.\n";
   my $dollarless = $1;
   my $loc = "$indic_here/$dollarless";
   if (exists $$yml{$dollarless}) {
      my $resolved = $$yml{$dollarless}; # All we know is that $resolved
      if (is_nonref($resolved)) {
         if ($type eq "search") {
            # In the case of "search" we expect $resolved to be aliases/names that
            # need to be resolved once through the alias map. This must work.
            my $e;
            ($e,$result) = resolve_placeholder_through_alias_map($dollarless,$resolved,$aliases_vss,$aliases_dss,$indic_here);
            $error_count += $e
         }
         elsif ($type eq "restart") {
            # In the case of "restart" we expect integers or (in the case of the base
            # for "restart by geometric sequence") floats. We have at least strings, let's
            # check them as a complimentary service, but leave it to MiniZinc to perform deeper
            # checks.
            if ($resolved =~ /^[0-9]*\.[0-9]+$/ || $resolved =~ /^[0-9]+$/) {
               $result = $resolved*1
            }
            else {
               print STDERR "Expected an integer or a float as the value associated to '$placeholder' at '$indic_here', but got '$resolved'\n";
               $error_count++
            }
         }
         else {
            die "Neither 'search' nor 'restart'"
         }
      }
      else {
         print STDERR "Whatever is stored under '$loc' is not a string.\n";
         $error_count++
      }
   }
   else {
      print STDERR "There is no entry '$loc' to resolve '$placeholder'.\n";
      $error_count++
   }
   return ($error_count,$result)
}

sub resolve_placeholder_through_alias_map {
   my($dollarless,$resolved,$aliases_vss,$aliases_dss,$indic_here) = @_;
   my $error_count = 0;
   my $alias_map;
   my $alias_type;
   my $result;
   if ($dollarless =~ /vss/) {
      $alias_map  = $aliases_vss;
      $alias_type = "VSS";
   }
   else {
      die "Should contain 'dss'" unless ($dollarless =~ /dss/);
      $alias_map  = $aliases_dss;
      $alias_type = "DSS";
   }
   if (exists $$alias_map{$resolved}) {       # and it must exist in the selected "alias map", i.e. be known
      $result = $$alias_map{$resolved}
   }
   else {
      print STDERR "The placeholder '$dollarless' at '$indic_here' maps to '$resolved', but '$resolved' is not in the '$alias_type' alias map.\n";
      $error_count++
   }
   return ($error_count,$result)
}

