package (>>>FILE_SANS<<<);

use strict;
use warnings;

=pod

=head1 NAME

(>>>FILE_SANS<<<) - (>>>synopsis<<<)

=head1 SYNOPSIS

  Quick code sample should go here...

=head1 METHODS

The methods for this module are listed here:

=over 4

=cut

#============================================================================#

=pod

=item I<Public>: B<new>

Create a new instance of (>>>FILE_SANS<<<) and then call initialise
on it.

=cut

sub new {
  my $class = shift;

  #-----------------------------#
  # Don't change this method    #
  # Change 'initialise' instead #
  #-----------------------------#

  my $self  = bless {}, $class;  
  $self->initialise(@_);
  return $self;
}

#============================================================================#

=pod

=item I<Private>: B<initialise>

Initialise this instance of this class.

=cut

sub initialise {
  my $self = shift;
  my %args = @_;
  
  (>>>POINT<<<)
}

#============================================================================#

=pod

=back 4

=head1 AUTHOR

(>>>USER_NAME<<<), (>>>DATE<<<)

=cut

#============================================================================#
#Return value of true so that this file can be used as a module.
1;
>>>TEMPLATE-DEFINITION-SECTION<<<
("synopsis" "Short Module Summary: ")
