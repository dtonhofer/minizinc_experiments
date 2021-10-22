#!/usr/bin/perl

######
# A module for a program which runs MiniZinc repeatedly on the same model with
# different data files and different search annotations (to apply different
# variable selection strategies and domain splitting strategies), and collects
# information about the run in a CSV file
#
# This module deals with reading a YAML file that provides configuration
# information.
#
# The homepage for this script with additional explanation is at
# https://github.com/dtonhofer/minizinc_experiments/tree/main/stats_collection
######

package Mzn::ReadYaml;

use strict;
use warnings;
use utf8;  # Meaning "This lexical scope (i.e. file) contains utf8"

use YAML::Tiny;     # install with "dnf install perl-YAML-Tiny"
use Data::Dumper;   # install with "dnf install perl-Data-Dumper"
use File::Basename;

use Exporter qw(import);
our @EXPORT_OK =
   qw(
      read_and_interprete_yaml
     );

my $aliases_indic      = 'aliases';
my $configs_indic      = 'configurations';
my $vss_indic          = 'variable_selection_strategy';
my $dss_indic          = 'domain_splitting_strategy';
my $rounds_indic       = 'rounds';
my $datafiles_indic    = 'datafiles';
my $annotation_indic   = 'annotation';
my $ann_name_indic     = 'name';
my $ann_template_indic = 'template';
my $limit_indic        = 'limit_s';
my $defaults_indic     = 'defaults';
my $modelfiles_indic   = 'modelfiles';
my $obj_name_indic     = 'obj_name';

sub read_and_interprete_yaml {
   my($args) = @_;
   my $yml = read_yaml_configfile($$args{configfile});
   if ($$args{debug_config}) {
      print STDERR "Reading YAML file yields the following:\n";
      print STDERR Data::Dumper->new([$yml])->Sortkeys(1)->Dump;
   }
   my $error_count = interprete_yml($yml,$args);

   if ($$args{debug_config}) {
      print STDERR "Variable selection strategy aliases are now:\n";
      print STDERR Data::Dumper->new([$$args{aliases_vss}])->Sortkeys(1)->Dump;
      print STDERR "Domain splitting strategy aliases are now:\n";
      print STDERR Data::Dumper->new([$$args{aliases_dss}])->Sortkeys(1)->Dump;

      print STDERR "Configurations\n";
      print STDERR Data::Dumper->new([$$args{configs}])->Sortkeys(1)->Dump;

      print STDERR "Data files\n";
      print STDERR Data::Dumper->new([$$args{datafiles}])->Sortkeys(1)->Dump;
      print STDERR "Model files\n";
      print STDERR Data::Dumper->new([$$args{modelfiles}])->Sortkeys(1)->Dump;
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

sub interprete_yml {
   my($yml,$args) = @_;
   my $error_count = 0;
   $error_count += interprete_aliases($yml,$$args{aliases_vss},$$args{aliases_dss});
   $error_count += interprete_configs($yml,$args);
   $error_count += interprete_datafiles($yml,$args);
   $error_count += interprete_modelfiles($yml,$args);
   return $error_count
}

sub is_hashref {
   my($maybe_ref_to_hash) = @_;
   return (ref $maybe_ref_to_hash eq ref {})
}

sub is_arrayref {
   my($maybe_ref_to_array) = @_;
   return (ref $maybe_ref_to_array eq ref [])
}

sub is_string {
   my($maybe_string) = @_;
   return (ref $maybe_string eq ref "")
}

sub interprete_aliases {
   my($yml,$aliases_vss,$aliases_dss) = @_;
   my $error_count = 0;
   if (exists $$yml{$aliases_indic}) {
      my $subyml = $$yml{$aliases_indic};
      if (is_hashref($subyml)) {
         $error_count += interprete_aliases_2($subyml,$aliases_vss,$aliases_dss,$aliases_indic);
      }
      else {
         print STDERR "Whatever is stored under '$aliases_indic' is not a hash.\n";
         $error_count++
      }
   }
   else {
      print STDERR "Warning: there is no data for '$aliases_indic'!\n";
   }
   return $error_count;
}

sub interprete_aliases_2 {
   my($yml,$aliases_vss,$aliases_dss,$indic_here) = @_;
   my $error_count = 0;
   # These change the "aliases_vss" and "aliases_dss" in-place
   my $e1 = interprete_aliases_3($yml,$vss_indic,$aliases_vss,"VSS",$indic_here);
   my $e2 = interprete_aliases_3($yml,$dss_indic,$aliases_dss,"DSS",$indic_here);
   return $e1 + $e2;
}

sub interprete_aliases_3 {
   my($yml,$vod_indic,$vod_aliases,$vod_type,$indic_here) = @_;
   my $error_count = 0;
   if (exists $$yml{$vod_indic}) {
      my $subyml = $$yml{$vod_indic};
      my $loc = "$indic_here/$vod_indic";
      if (is_hashref($subyml)) {
         $error_count += interprete_aliases_4($subyml,$vod_aliases,$vod_type,$loc);
      }
      else {
         print STDERR "Whatever is stored under '$loc' is not a hash.\n";
         $error_count++
      }
   }
   else {
      # Nothing to do as there are no such aliases
   }
   return $error_count
}

sub interprete_aliases_4 {
   my($yml,$vod_aliases,$vod_type,$indic_here) = @_;
   my $error_count = 0;
   my $new_aliases = {}; # work on a temporary map before doing in-place replacement
   ($$new_aliases{$_} = $$vod_aliases{$_}) for keys %$vod_aliases; # copy over
   for my $alias_key (keys %$yml) {
      my $alias_val = $$yml{$alias_key};
      if (is_string($alias_val)) { 
         if (!exists $$new_aliases{$alias_val}) {
            # There should be an entry for $alias_val in $aliases because
            # because all the allowed (non-alias) keywords are (presumably)
            # already in there, mapping to themselves.
            # Emit a warning!
            print STDERR "Warning: In the configuration, the $vod_type aliases contain the mapping '$alias_key' => '$alias_val', but '$alias_val' is unknown.\n"; 
         }
         if (exists $$new_aliases{$alias_key}) { 
            # Really only possible if the program has set one up as it can come from the YAML
            # which disallows multiple same keys
            print STDERR "An entry for '$alias_key' already exists in the $vod_type aliases.\n";
            print STDERR "Existing value : '$$new_aliases{$alias_key}'.\n";
            print STDERR "New value      : '$alias_val'.\n";
         }
         $$new_aliases{$alias_key} = $alias_val;
      }
      else {
         print STDERR "Whatever is stored under '$indic_here/$alias_key' is not a string.\n";
         $error_count++
      }
   }
   if ($error_count == 0) {
      ($$vod_aliases{$_} = $$new_aliases{$_}) for keys %$new_aliases; # copy over 
   }
   return $error_count
}

sub interprete_configs {
   my($yml,$args) = @_;
   my $error_count = 0;
   if (exists $$yml{$configs_indic}) {
      my $subyml = $$yml{$configs_indic};
      if (is_hashref($subyml)) {
         $error_count += interprete_configs_2($subyml,$args,$configs_indic)
      }
      else {
         print STDERR "Whatever is stored under '$configs_indic' is not a hash.\n";
         $error_count++
      }
   }
   else {
      print STDERR "Warning: there is no data for '$configs_indic'!\n";
   }
   return $error_count
}

sub interprete_configs_2 {
   my($yml,$args,$indic_here) = @_;
   my $error_count = 0;
   for my $config_name (keys %$yml) {
      if (exists $$args{configs}{$config_name}) { # only possible if the program has a hardcoded config
         print STDERR "There already is a configuration '$config_name'.\n";
         $error_count++
      }
      else {
         my $subyml = $$yml{$config_name};
         my $loc = "$indic_here/$config_name";
         if (is_hashref($subyml)) {
            my ($e,$single_config) = interprete_single_config($subyml,$args,$loc);
            $error_count += $e;
            if ($e == 0) {
               $$args{configs}{$config_name} = $single_config
            }
         }
         else {
            print STDERR "Whatever is stored under configuration '$loc' is not a hash.\n";
            $error_count++
         }
      }
   }
   return $error_count
}

sub interprete_single_config {
   my($yml,$args,$indic_here) = @_;
   my $single_config = {};
   my $error_count = 0;
   #
   # The config must hold one "annotation" element carrying the annotation name under 
   # "name" and the annotation template under "template". Like this:
   #
   #  annotation:
   #     name:       s_annotation
   #     template:   int_search(s, $vss, $dss)
   #
   # It is an error if it is missing.
   #
   my ($e1,$ann_name,$ann_template) = interprete_annotation($yml,$indic_here);
   $error_count += $e1;
   if ($e1 == 0) {
     $$single_config{ann}{name}     = $ann_name;     
     $$single_config{ann}{template} = $ann_template; 
     die "no annotation name"     unless $ann_name;     # assertion
     die "no annotation template" unless $ann_template; # assertion
   }
   # 
   # The config may hold defaults for "rounds", "limit_s", "obj_name".
   # The whole "defaults" block may be missing or one or the other
   # value may be missing. They can be overriden in individual strategies of
   # the config. The "defaults" hash will contain the "superdefaults"
   # overridden by any defaults found
   #
   my $superdefaults = {
      rounds   => 1
     ,limit_s  => 0      # no limit!
     ,obj_name => "obj"
   };
   my ($e2,$defaults) = interprete_defaults($yml,$superdefaults,$indic_here);
   $error_count += $e2;
   # 
   # Defaults will be inserted into the individual "strategy" hashes so that
   # later there is everything one needs into those hashes.
   # Only continue if no error so far.
   #
   if ($error_count == 0) {
      my($e3,$strategies) = interprete_strategies($yml,$defaults,$ann_template,$ann_name,$args,$indic_here);
      $error_count += $e3;
      if ($e3 == 0) {
         $$single_config{strategies} = $strategies
      }
   }
   return ($error_count,$single_config)
}

sub interprete_defaults {
   my($yml,$superdefaults,$indic_here) = @_;
   my $defaults = {};
   my $error_count = 0;
   ($$defaults{$_} = $$superdefaults{$_}) for keys %$superdefaults; # copy superdefaults into defaults
   my $loc = "$indic_here/$defaults_indic";
   if (exists $$yml{$defaults_indic}) {
      my $defaults_yml = $$yml{$defaults_indic};
      if (is_hashref($defaults_yml)) {
         read_overrides($defaults_yml,$defaults,$loc); # override values in %$defaults
      }
      else {
         print STDERR "Whatever is stored under '$loc' is not a hash.\n";
         $error_count++;
      }
   }
   else {
      print STDERR "Nothing found under '$loc'. Using superdefaults.\n";
   }
   return ($error_count,$defaults);
}

sub interprete_strategies {
   my($yml,$defaults,$ann_template,$ann_name,$args,$indic_here) = @_;
   my $strategies = {};
   my $error_count = 0;
   for my $strategy_name (keys %$yml) {
      if ($strategy_name eq $annotation_indic ||
          $strategy_name eq $defaults_indic) {
         # skip these as they are not strategies
         next
      }
      else {
         my $subyml = $$yml{$strategy_name};
         my $loc = "$indic_here/$strategy_name";
         if (is_hashref($subyml)) {
            my ($e,$single_strategy) = interprete_single_strategy($subyml,$defaults,$ann_template,$ann_name,$args,"$loc");
            $error_count += $e;
            if ($e == 0) {
               $$strategies{$strategy_name} = $single_strategy
            }
         }
         else {
            print STDERR "Whatever is stored under strategy '$loc' is not a hash.\n";
            $error_count++
         }
      }
   }
   return ($error_count,$strategies)
}

sub interprete_single_strategy {
   my($yml,$defaults,$ann_template,$ann_name,$args,$indic_here) = @_;
   my $strategy;
   my $nondefaults = {};
   my $error_count = 0;
   ($$nondefaults{$_} = $$defaults{$_}) for keys %$defaults; # copy defaults into nondefaults
   read_overrides($yml,$nondefaults);
   my($e1,$ann_filled) = replace_in_template($yml,$ann_template,$args,$indic_here);
   $error_count = $error_count + $e1;
   if ($e1 == 0) {
      $strategy = {
         annotation  => $ann_filled
        ,ann_name    => $ann_name
        ,rounds      => $$nondefaults{rounds}
        ,limit_s     => $$nondefaults{limit_s}
        ,obj_name    => $$nondefaults{obj_name}
      }
   }
   return ($error_count,$strategy)
}

sub read_overrides {
   my($yml,$defaults) = @_;
   if (exists $$yml{$rounds_indic}) {
      my $x = parse_rounds($$yml{$rounds_indic});
      if (defined $x) { $$defaults{rounds} = $x } # override
   }
   if (exists $$yml{$limit_indic}) {
      my $x = parse_limit($$yml{$limit_indic});
      if (defined $x) { $$defaults{limit_s} = $x } # override
   }
   if (exists $$yml{$obj_name_indic}) {
      my $x = $$yml{$obj_name_indic};
      if ($x) { $$defaults{obj_name} = $x } # override
   }
}

sub parse_rounds {
   my($value) = @_;
   if ($value =~ /^[0-9]+$/) {
      return($value * 1)
   }
   else {
      print STDERR "Rounds value '$value' looks bad. Ignored!\n";
      return undef
   }
}

sub parse_limit {
   my($value) = @_;
   if ($value =~ /^[0-9]+$/) {
       return($value * 1)
   }
   else {
      print STDERR "Limit value '$value' looks bad. Ignored!\n";
      return undef
   }
}

sub replace_in_template {
   my($yml,$template,$args,$indic_here) = @_;
   # just kickstart the recursion
   return replace_in_template_2($yml,$template,$template,$args,$indic_here);
}

sub replace_in_template_2 {
   my($yml,$template,$orig_template,$args,$indic_here) = @_;
   my $error_count = 0;
   my $filled;
   my $mappings = {};
   if ($template =~ /^(.*?)(\$[A-Za-z][A-Za-z0-9_]*)(.*?)$/) {
      my $before      = $1;
      my $placeholder = $2;
      my $after       = $3;
      # a bit awkwardly, we demand that the placeholder contain "vss" or "dss" to be able to
      # find out through which alias map the value for 'placeholder' in $yml can be de-aliased
      if ($placeholder =~ /(vss|dss)/) {
         # recursive call may collect more errors
         my($e1,$subfilled) = replace_in_template_2($yml,$after,$orig_template,$args,$indic_here); # RECURSE
         my($e2,$resolved)  = resolve_placeholder($yml,$placeholder,$args,$indic_here);
         $error_count = $error_count + $e1 + $e2;
         if ($e1 + $e2 == 0) {
            $filled = $before . $resolved . $subfilled;
         }
     }
      else {
         print STDERR "The placeholder '$placeholder' in '$indic_here' does not contain the substring 'vss' or 'dss', but I need it to resolve through the VSS or DSS alias map.\n";
         $error_count++
      }
   }
   elsif ($template =~ /\$/) {
      print STDERR "Leftover '\$' that is not part of a placeholder in template '$orig_template' at '$template' in '$indic_here'\n";
      $error_count++
   }
   else {
      $filled = $template
   }
   return ($error_count,$filled);
}

sub resolve_placeholder {
   my($yml,$placeholder,$args,$indic_here) = @_;
   my $error_count = 0;
   my $result;
   $placeholder =~ /^\$(.+)$/ or die "Placeholder '$placeholder' does not match '\$X'.\n";
   my $dollarless = $1;
   my $loc = "$indic_here/$dollarless";
   if (exists $$yml{$dollarless}) {              # the dollarless place holdername must exist as key
      my $alias = $$yml{$dollarless};
      if (is_string($alias)) {                   # and it must be a string!
         my $alias_map;
         my $alias_type;
         if ($dollarless =~ /vss/) {
            $alias_map  = $$args{aliases_vss};   
            $alias_type = "VSS";
         }
         else {
            die "Should contain 'dss'" unless ($dollarless =~ /dss/);
            $alias_map  = $$args{aliases_dss};
            $alias_type = "DSS";
         }
         if (exists $$alias_map{$alias}) {       # and it must exist in the selected "alias map", i.e. be known
            $result = $$alias_map{$alias}
         }
         else {
            print STDERR "The placeholder '$loc' has no $alias_type alias.\n";
            $error_count++
         }
      }
      else {
         print STDERR "Whatever is stored under '$loc' is not a string.\n";
         $error_count++
      }
   }
   else {
      print STDERR "There is no entry '$loc' as required by the template.\n";
      $error_count++
   }
   return ($error_count,$result)
}

# We expect something like this:
#
#  annotation:
#     name:       s_annotation
#     template:   int_search(s, $vss, $dss)

sub interprete_annotation {
   my($yml,$indic_here) = @_;
   my $error_count = 0;
   my $ann_template;
   my $ann_name;
   my $loc = "$indic_here/$annotation_indic";
   if (exists $$yml{$annotation_indic}) {
      my $ann_yml = $$yml{$annotation_indic};
      if (is_hashref($ann_yml)) {
         my ($e1,$e2);
         ($e1,$ann_name)     = read_annotation_name($ann_yml,$loc);
         ($e2,$ann_template) = read_annotation_template($ann_yml,$loc);
         $error_count = $error_count + $e1 + $e2;
      }
      else {
         print STDERR "Whatever is stored under '$loc' is not a hash.\n";
         $error_count++
      }
   }
   else {
      print STDERR "The configuration at '$loc' contains no annotation information.\n";
      $error_count++
   }
   return ($error_count,$ann_name,$ann_template);
}

sub read_annotation_name {
   my($ann_yml,$indic_here) = @_;
   my $error_count = 0;
   my $ann_name;
   my $loc = "$indic_here/$ann_name_indic";
   if (exists $$ann_yml{$ann_name_indic}) {
      $ann_name = $$ann_yml{$ann_name_indic};
      if ($ann_name =~ /^[A-Za-z][A-Za-z0-9_]*$/) {
         # Success, it is a name
      }
      else {
         print STDERR "Stuff found at '$loc' does not look like a name.\n";
         $error_count++
      }
   }
   else {
      print STDERR "There is no annotation name at '$loc'.\n";
      $error_count++
   }
   return ($error_count,$ann_name)
}

sub read_annotation_template {
   my($yml,$indic_here) = @_;
   my $error_count = 0;
   my $ann_template;
   my $loc = "$indic_here/$ann_template_indic";
   if (exists $$yml{$ann_template_indic}) {
      $ann_template = $$yml{$ann_template_indic};
      # Success, no further checks
   }
   else {
      print STDERR "There is no annotation template at '$loc'.\n";
      $error_count++
   }
   return ($error_count,$ann_template)
}

sub interprete_modelfiles {
   my($yml,$args) = @_;
   my $error_count = 0;
   if (exists $$yml{$modelfiles_indic}) {
      my $array = $$yml{$modelfiles_indic}; # for now, an array
      if (is_arrayref($array)) {
         my $modelfiles = interprete_modelfiles_2($array,$args,$modelfiles_indic);
         my $existing = $$args{modelfiles};
         push @$existing, @$modelfiles; # push to the existing array
      }
      else {
         print STDERR "Whatever is stored under '$modelfiles_indic' is not an array.\n";
         $error_count++
      }
   }
   else {
      print STDERR "Warning: there is no data for '$modelfiles_indic'!\n";
   }
   return $error_count
}

sub interprete_modelfiles_2 {
   my($array,$args,$indic_here) = @_;
   my $res = [];
   for my $filename (@$array) {
      my $abs_filename;
      if (File::Spec->file_name_is_absolute($filename)) {
         $abs_filename = $filename;
      }
      else {
         $abs_filename = File::Spec->catfile($$args{model_dir},$filename)
      }
      if (-f $abs_filename) {
         push @$res, $abs_filename;
      }
      else {
         print STDERR "The model file '$abs_filename' does not exist -- skipping this!\n";
      }
   }
   return $res # an arrayref
}

sub interprete_datafiles {
   my($yml,$args) = @_;
   my $error_count = 0;
   if (exists $$yml{$datafiles_indic}) {
      my $subyml = $$yml{$datafiles_indic};
      if (is_hashref($subyml)) {
         $error_count += interprete_datafiles_2($subyml,$args,$datafiles_indic)
      }
      else {
         print STDERR "Whatever is stored under '$datafiles_indic' is not a hash.\n";
         $error_count++
      }
   }
   else {
      print STDERR "Warning: there is no data for '$datafiles_indic'!\n";
      # If a datafile is not actually needed for the model, one would need to define a dummy.
      # One way to avoid that would be to have a "nodatafiles" entry mapping to the
      # appropriate config.
   }
   return $error_count
}

sub interprete_datafiles_2 {
   my($yml,$args,$indic_here) = @_;
   my $error_count = 0;
   for my $filename (keys %$yml) {
      my $abs_filename;
      if (File::Spec->file_name_is_absolute($filename)) {
         # only happens if the hash key looks like an absolute filename
         $abs_filename = $filename
      }
      else {
         $abs_filename = File::Spec->catfile($$args{data_dir},$filename)
      }
      my $base_filename = basename($abs_filename);
      if (-f $abs_filename) {
         # the $filename is the key, the config name is the value
         my $config_name = $$yml{$filename};
         if (exists $$args{configs}{$config_name}) {
            $$args{datafiles}{$abs_filename} = $config_name;
         }
         else {
            print STDERR "There is no configuration '$config_name' as requested for data file '$filename'.\n";
            $error_count++
         }
      }
      else {
         print STDERR "The data file '$abs_filename' does not exist -- skipping that file!\n";
      }
   }
   return $error_count
}


