#!/usr/bin/perl

###############################################################################
# Some routines used from various places.
#
# The homepage for this script with additional explanation is at
# https://github.com/dtonhofer/minizinc_experiments/tree/main/stats_collection
###############################################################################

package Mzn::Helpers;

use strict;
use warnings;
use utf8;  # Meaning "This lexical scope (i.e. file) contains utf8"

use Clone qw(clone);

use Exporter qw(import);
our @EXPORT_OK = qw(is_hashref is_arrayref is_nonref clone_and_merge);

sub is_hashref {
   my($maybe_ref_to_hash) = @_;
   return (ref $maybe_ref_to_hash eq ref {}) # ref {} resolves to "HASH"
}

sub is_arrayref {
   my($maybe_ref_to_array) = @_;
   return (ref $maybe_ref_to_array eq ref []) # ref [] resolves to "ARRAY"
}

sub is_nonref {
   my($maybe_nonref) = @_;
   return (ref $maybe_nonref eq "") # ref of a non-ref (string,number,list,hash) resolves to ""
}

# merge the "$underlay" and the "$overlay" into a new and cloned structure that is returned

sub clone_and_merge {
   my($underlay,$overlay) = @_;
   # we make sure that we have our own private copies of everything by cloning both sides
   my $underlay2 = clone($underlay);
   my $overlay2  = clone($overlay);
   return merge_precloned($underlay2,$overlay2);
}

sub merge_precloned {
   my($underlay,$overlay) = @_;
   if (!defined $underlay && !defined $overlay) {
      return undef
   }
   elsif (!defined $underlay) {
      return $overlay
   }
   elsif (!defined $overlay) {
      return $underlay
   }
   else {
      if (is_hashref($underlay) && is_hashref($overlay)) {
         for my $key (keys %$overlay) {
            if (exists $$underlay{$key}) {
               # may change $$underlay{$key} in-place, but also returns it
               # so that it can be inserted (needed if the value is a scalar)
               $$underlay{$key} = merge_precloned($$underlay{$key},$$overlay{$key})
            }
            else {
               $$underlay{$key} = $$overlay{$key}
            }
         }
         return $underlay
      }
      elsif (is_arrayref($underlay) && is_arrayref($overlay)) {
         die "Merging two array references has not been implemented"
      }
      elsif (is_nonref($underlay) && is_nonref($overlay)) {
         # These could be strings, numbers, lists, hashes - but not references
         # Just return the "overlay"
         return $overlay
      }
      else {
         my $under_ref = ref $underlay;
         my $over_ref  = ref $overlay;
         die "Merging two things that are not the same: '$under_ref' and '$over_ref'";
      }
   }
}

