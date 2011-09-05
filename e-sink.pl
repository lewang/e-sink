#!/usr/bin/env perl

## Use IO::Select to read STDIN without blocking

use strict;
use warnings;
use POSIX qw(ARG_MAX tmpnam);
use IO::Select;

use vars qw($EMACSCLIENT $BUFFER_TITLE $TEMP_FILE $TEMP_FILE_H);

sub esc_chars($) {
  # will change, for example, a!!a to a\!\!a
  my ($str) = @_;
  if ($str) {
    $str =~ s/([\\;<>\*\|`&\$!#\(\)\[\]\{\}:'"])/\\$1/g;
  } else {
    die "why no str?";
  }
  $str;
}

sub get_command_arr($) {
  my ($data) = @_;
  ('--no-wait', '--eval', <<AARDVARK)
(e-sink-receive "$BUFFER_TITLE" "$data")
AARDVARK
}

sub push_data_to_emacs($) {
  my ($data) = @_;
  if ($TEMP_FILE) {
    print $TEMP_FILE_H $data;
  } else {
    system($EMACSCLIENT, get_command_arr($data));
    0 == $? || die "\nfailed to push data to Emacs";
  }
}

sub emacs_start_e_sink() {
  system($EMACSCLIENT, "--no-wait", "--eval", <<AARDVARK);
(progn (require 'e-sink) (e-sink-start "${BUFFER_TITLE}"))
AARDVARK
  0 == $? || die "\nunable to open new frame\n\nconfirm emacs server started.";
}

sub emacs_finish_e_sink() {
  if ($TEMP_FILE) {
    system($EMACSCLIENT, "--no-wait", "--eval", <<AARDVARK);
(e-sink-insert-and-finish "$BUFFER_TITLE" "$TEMP_FILE")
AARDVARK
  } else {
    system($EMACSCLIENT, "--no-wait", "--eval", <<AARDVARK);
(e-sink-finish "$BUFFER_TITLE")
AARDVARK
  }
  0 == $? || die "\nunable to send finish tag.";
}

sub print_help() {
  print <<AARDVARK
Usage: $0 [OPTION]... [buffer-name]

  -t use temporary file instead of command-line to pass data
  -h this screen

AARDVARK
}

sub main() {

  for my $i ( 0..$#ARGV ) {
    if ( grep /$ARGV[$i]/, ("--help", "-h") ) {
      print_help();
      exit(0);
    } elsif ( $ARGV[$i] eq "-t" ) {
      $TEMP_FILE= tmpnam();
      delete $ARGV[$i];
    }
  }
  @ARGV= grep(defined, @ARGV);

  if ( scalar(@ARGV) > 1) {
    print STDERR "unexpected '$ARGV[1]'";
    exit(1);
  }

  ### start non-blocking read ASAP
  my $s = IO::Select->new;
  $s->add(\*STDIN);

  $EMACSCLIENT= "emacsclient";
  $BUFFER_TITLE= ($ARGV[0]? $ARGV[0] : '');

  close(STDOUT);                # getting output from Emacs might be confusing

  emacs_start_e_sink();

  my $arg_max;

  if ($TEMP_FILE) {
    $arg_max= 100_000;            #
  } else {
    my $temp= join('', get_command_arr(''));
    $arg_max= ARG_MAX - length($temp) - 1; # 1 for NULL string end
  }

  my $data;
  $TEMP_FILE and open($TEMP_FILE_H, ">$TEMP_FILE");

  while ( $s->can_read() ) {
    my $line= <STDIN>;

    if ( ! $line ) {
      last;
    }

    unless ($TEMP_FILE) {
      $line= esc_chars( $line );
    }

    if ( $data && ( length($data) + length($line) > $arg_max) ) {
      push_data_to_emacs( $data );
      $data= $line;
    } else {
      $data .= $line;
    }
  }

  $data and push_data_to_emacs( $data );
  $TEMP_FILE_H and close($TEMP_FILE_H);
  emacs_finish_e_sink;
  0;
}


main;
