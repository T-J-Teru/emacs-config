#!/usr/bin/perl

use warnings;
use strict;

#========================================================================#

=pod

=head1 NAME

install.pl - Install links from this directory to ~/.emacs.d/

=head1 OPTIONS

B<install.pl> [-h|--help] [-p|--pretend] [-v|--verbose]

=head1 SYNOPSIS

Create links from the current directory to the ~/.emacs.d/ directory
to install the emacs configuration.

Will refuse to run if we're already in the .emacs.d directory.

With I<--pretend> don't actually create the links but will print any
warnings that would have been printed.

With I<--verbose> print more information about what is being done.

=cut

#========================================================================#

# --
# Standard Perl Modules.
use Cwd 'abs_path';
use File::Basename;
use Getopt::Long;

# --
# User Defined Modules.
use lib "$ENV{HOME}/lib";
use GiveHelp qw/usage/;         # Allow -h or --help command line options.

# First check to see if we're in the .emacs.d directory already.
my $script_filename = abs_path ($0);
my $script_dir = dirname ($script_filename);

my $emacs_dir = $ENV{HOME} . "/tmp/.emacs.d";

my $verbose = 0;
my $pretend = 0;
GetOptions ("verbose|v" => \$verbose,
            "pretend|p" => \$pretend);

if (not (-d $emacs_dir))
{
  if ($verbose)
  {
    print "Creating '$emacs_dir'\n";
  }
  
  if (not $pretend)
  {    
    mkdir $emacs_dir or
      die "Failed to create directory '$emacs_dir': $!";
  }
}

my @dont_install = load_dont_install_list ($script_dir);

install ($emacs_dir, $script_dir, @dont_install);

#========================================================================#

=pod

=head1 METHODS

The following methods are defined in this script.

=over 4

=cut

#========================================================================#

=pod

=item B<install>

Currently undocumented.

=cut

sub install {
  my $dest = shift;
  my $src  = shift;  
  my @dont_install = @_;

  opendir my $dh, $src or
    die "Failed to open directory '$src': $!";
  
  my @contents = readdir $dh;
  
  closedir $dh or
    die "Failed to close directory '$src': $!";
  
  foreach my $file (@contents)
  {
    next if ($file =~ m/^\./);
    next if ($file =~ m/~$/);
    next if (is_file_in_dont_install_list ($file, @dont_install));

    my $old = $src . "/" . $file;
    my $new = $dest . "/" . $file;

    if (-l $new)
    {
      my $link = readlink $new or
        die "Failed to readlink '$new': $!";
      
      if (abs_path ($link) eq abs_path ($old))
      {
        if ($verbose)
        {
          print "File '$file' already installed.\n";
        }
      }
      else
      {
        warn "File '$new' already exists, can't install '$file'\n";
      }
    }
    elsif (-d $old)
    {
      if (-e $new and (not -d $new))
      {
        warn "Directory '$new' can't be created.\n";
      }
      elsif (not -e $new)
      {
        if ($verbose)
        {
          print "Creating directory '$new'\n";
        }
        
        if (not $pretend)
        {
          mkdir $new or
            die "Failed to create directory '$new': $!";
        }
      }
      
      install ($new, $old, @dont_install);
    }
    else
    {      
      if ($verbose)
      {
        print "Creating link '$old' -> '$new'\n";
      }
      
      if (not $pretend)
      {      
        symlink $old, $new or 
          die "Failed to create symlink '$old' -> '$new': $!";
      }
    }
  }
}

#========================================================================#

=pod

=item B<is_file_in_dont_install_list>

Currently undocumented.

=cut

sub is_file_in_dont_install_list {
  my $file = shift;
  my @list = @_;
  
  foreach my $re (@list)
  {
    if ($file =~ m/^$re$/)
    {
      return 1;
    }
  }
  
  return 0;
}

#========================================================================#

=pod

=item B<load_dont_install_list>

Currently undocumented.

=cut

sub load_dont_install_list {
  my $dir = shift;
  
  my $dont_install_file = $dir . "/no-install";
  my @no_install = ();

  if (-f $dont_install_file)
  {
    if ($verbose)
    {
      print "Loading don't install file from: $dont_install_file\n";
    }
    
    open my $fh, $dont_install_file or
      die "Failed to open '$dont_install_file': $!";
    
    while (<$fh>)
    {
      chomp;
      if ($verbose)
      {
        print "  Regexp: ^$_\$\n";
      }

      push @no_install, $_;
    }

    if ($verbose)
    {
      print "All loaded.\n\n";
    }

    close $fh or
      die "Failed to close '$dont_install_file': $!";
  }
  elsif ($verbose)
  {
    print "No don't install file at: $dont_install_file\n";
    print "...that's ok though, installing everything...\n\n";
  }
  
  return @no_install;
}

#========================================================================#

#========================================================================#

=pod

=back 4

=head1 AUTHOR

Andrew Burgess, 14 Jun 2011

=cut
