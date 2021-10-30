#!/usr/bin/perl

###############################################################################
# Filling the result file.
#
# The homepage for this script with additional explanation is at
# https://github.com/dtonhofer/minizinc_experiments/tree/main/stats_collection
###############################################################################

package Mzn::ResultFiling;

use strict;
use warnings;
use utf8;  # Meaning "This lexical scope (i.e. file) contains utf8"

use Fcntl qw(:flock SEEK_END);

use Exporter qw(import);
our @EXPORT_OK =
   qw(
       touch_resultfile
       rename_resultfile_if_exists
       write_to_resultfile
     );

sub rename_resultfile_if_exists {
   my($resultfile) = @_;
   if (-e $resultfile) {
      print STDERR "The result file '$resultfile' exists -- renaming it!\n";
      my $index = 1;
      my $bak_name;
      do {
         $bak_name = "${resultfile}.${index}"; # I'm not happy with this way of naming, one should SHIFT the indexes instead
         $index++;
      } until (! -e $bak_name);
      rename($resultfile,$bak_name) || die "Could not rename file '$resultfile' to '$bak_name': $!";
      print STDERR "Renamed the existing result file to '$bak_name'\n"
   }
}

sub get_resultfile_line {
   return {
       1  => {  name => "model"
               ,print => 1
               ,format => '%15s'
               ,comment => "the extension-stripped basename of the model file" }
      ,2  => {  name => "data"
               ,print => 1
               ,format => '%15s'
               ,comment => "the extension-stripped basename of the data file" }
      ,3  => {  name => "config_name"
               ,print => 1
               ,format => '%15s'
               ,comment => "the name of the 'configuration'" }
      ,4  => {  name => "application_name"
               ,print => 1
               ,format => '%15s'
               ,comment => "the name of the 'application' inside the 'configuration'" }
      ,5  => {  name => "search"
               ,format => '%15s'
               ,print => 1
               ,comment => "full search annotation text (can be long, can be missing)" }
      ,6  => {  name => "restart"
               ,format => '%15s'
               ,print => 1
               ,comment => "full restart annotation text (can be long, can be missing)" }
      ,7  => {  name => "round"
               ,format => '%3d'
               ,print => 1
               ,comment => "the index of the 'round' (starting from 1) if exactly the same problem is run several times" }
      ,8  => {  name => "rounds"
               ,format => '%3d'
               ,print => 1
               ,comment => "the number of rounds expected" }
      ,9  => {  name => "limit_s"
               ,format => '%5d'
               ,print => 1
               ,comment => "time limit given to MiniZinc in seconds; the best solution (in this case, shortest schedule solution) found within that limit is retained" }
      ,10 => {  name => "duration_s"
               ,format => '%5d'
               ,print => 1
               ,comment => "time spent processing in seconds, as determined by the collection script" }
      ,11 => {  name => "obj"
               ,format => '%4d'
               ,print => 1
               ,comment => "best objective value found (problem-dependent numeric scalar). If nothing was found, we write 'NA' (as is the custom in 'R')" }

      # The following have been parsed from MiniZinc statistics output.
      # Refer to https://www.minizinc.org/doc-2.5.5/en/fzn-spec.html#statistics-output for the MiniZinc performance values.

      ,12 => {  name => "minizinc.init_time_s"
               ,format => '%4d'
               ,print => 1
               ,comment => "time spent initializing in seconds" }
      ,13 => {  name => "minizinc.solve_time_s"
               ,format => '%6.2f'
               ,print => 1
               ,comment => "time spent solving in seconds. Cannot be larger than 'limit_s', but can be smaller" }
      ,14 => {  name => "minizinc.solutions"
               ,format => '%4d'
               ,print => 1
               ,comment => "number of solutions found during optimization. If this is 0, it's a bust!" }
      ,15 => {  name => "minizinc.variables"
               ,format => '%4d'
               ,print => 1
               ,comment => "number of variables created from the problem statement" }
      ,16 => {  name => "minizinc.propagators"
               ,format => '%4d'
               ,print => 1
               ,comment => "number of propagators" }
      ,17 => {  name => "minizinc.propagations"
               ,format => '%8d'
               ,print => 1
               ,comment => "number of propagator invocations" }
      ,18 => {  name => "minizinc.nodes"
               ,format => '%8d'
               ,print => 1
               ,comment => "number of search nodes" }
      ,19 => {  name => "minizinc.failures"
               ,format => '%8d'
               ,print => 1
               ,comment => "number of leaf nodes that were failed" }
      ,20 => {  name => "minizinc.restarts"
               ,format => '%4d'
               ,print => 1
               ,comment => "number of times the solver restarted the search (jumped back to the root search node)" }
      ,21 => {  name => "minizinc.peak_depth"
               ,format => '%4d'
               ,print => 1
               ,comment => "peak depth of search tree reached" }
      ,22 => {  name => "minizinc.num_solutions"
               ,format => '%2d'
               ,print => 1
               ,comment => "the 'nSolutions' value, probably 'number of solutions output'" }
   };
}

sub touch_resultfile {
   my($resultfile) = @_;
   open(my $fh,">",$resultfile) or die "Could not open file '$resultfile' for output: $!";
   my $line = get_resultfile_line();
   my $add_comma = 0;
   for my $key (sort { $a <=> $b } keys %$line) {
      if ($add_comma) { print $fh ", " } else { $add_comma = 1 }
      my $sub = $$line{$key};
      my $name = $$sub{name};
      if ($name =~ /^minizinc\.(.+)$/) {
         $name = $1 # Remove the "minizinc." prefix for brevity
      }
      print $fh $name;
   }
   print $fh "\n";
   close($fh) or die "Could not close file '$resultfile': $!";
}

sub write_to_resultfile {
   my($resultfile,$result) = @_;
   open(my $fh,">>",$resultfile) or die "Could not open file '$resultfile' for output: $!";
   lock($fh);
   my $line = get_resultfile_line();
   my $add_comma = 0;
   for my $key (sort { $a <=> $b } keys %$line) {
      if ($add_comma) { print $fh ", " } else { $add_comma = 1 }
      my $sub = $$line{$key};
      my $name = $$sub{name};
      if ($$sub{print}) {
         if (exists $$result{$name} && defined $$result{$name}) {
            my $value = $$result{$name};
            if (my $formatter = $$sub{formatter}) {
               # specialized routine
               my $formatted = $formatter->($value);
               print $fh $formatted
            }
            elsif (my $format = $$sub{format}) {
               # standard
               if ($format =~ /^%\d*s/) {
                  $value =~ /^\s*(.*?)\s*$/ or die "Could not match";
                  my $trimmed = $1;
                  my $quoted  = '"' . $trimmed . '"'; # what if trimmed contains double quotes?
                  printf $fh ($format,$quoted);
               }
               else {
                  printf $fh ($format, $value);
               }
            }
            else {
               print STDERR "Missing formatter or format for '$name'\n";
               print $fh $value
            }
         }
         else {
            print STDERR "Missing result value for '$name'\n";
            print $fh "NA";
         }
      }
      else {
         print STDERR "Not printing column '$key'\n"
      }
   }
   print $fh "\n";
   unlock($fh);
   close($fh) or die "Could not close file '$resultfile': $!";
}

sub lock {
    my ($fh) = @_;
    flock($fh, LOCK_EX) or die "Cannot lock: $!\n";
}

sub unlock {
    my ($fh) = @_;
    flock($fh, LOCK_UN) or die "Cannot unlock: $!\n";
}

