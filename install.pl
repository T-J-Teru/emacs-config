#!/usr/bin/perl

use warnings;
use strict;

#========================================================================#

=pod

=head1 NAME

install.pl - Install links from this directory to ~/.emacs.d/

=head1 OPTIONS

B<install.pl> [-h|--help] [-p|--pretend] [-v|--verbose] [--dir=<DIR>]

=head1 SYNOPSIS

Create links from the current directory to the ~/.emacs.d/ directory
to install the emacs configuration.

Will refuse to run if we're already in the .emacs.d directory.

With I<--pretend> don't actually create the links but will print any
warnings that would have been printed.

With I<--verbose> print more information about what is being done.

The destination directory can be changed from ~/.emacs.d/ using the
I<--dir> option.

=cut

#========================================================================#

use Cwd 'abs_path';
use File::Basename;
use Getopt::Long;
use Pod::Usage;

my $script_dir = dirname (abs_path ($0));
my $emacs_dir = undef;

my $verbose = 0;
my $pretend = 0;
my $help = 0;
GetOptions ("verbose|v" => \$verbose,
            "pretend|p" => \$pretend,
            "help|h"    => \$help,
            "dir|d=s"   => \$emacs_dir);

if ($help) 
{ 
  pod2usage ({-exitval => 0});
  exit(1);
}
 
if (not defined $emacs_dir)
{
  $emacs_dir = $ENV{HOME} . "/.emacs.d";
}

if (abs_path ($script_dir) eq abs_path ($emacs_dir))
{
  pod2usage ({-exitval => 1});
}

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

if ($verbose)
{
  print "Installing into '$emacs_dir'\n";
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
    elsif (-d $old and -d $new)
    {
      # The new directory already exists, recursively install into it.
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
