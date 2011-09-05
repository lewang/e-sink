#!/usr/bin/env perl

## Use IO::Select to read STDIN without blocking

use strict;
use warnings;
use POSIX qw(ARG_MAX);
use IO::Select;

use vars qw($EMACSCLIENT $BUFFER_TITLE);

sub esc_chars($) {
  # will change, for example, a!!a to a\!\!a
  my ($str) = @_;
  if ($str) {
    $str =~ s/([\\;<>\*\|`&\$!#\(\)\[\]\{\}:'"])/\\$1/g;
  } else {
    die "why no str?";
  }
}

sub get_command_arr($) {
  my ($data) = @_;
  ('--no-wait', '--eval', <<AARDVARK)
(e-sink-receive "$BUFFER_TITLE" "$data")
AARDVARK
}

sub push_data_to_emacs($) {
  my ($data) = @_;
  system($EMACSCLIENT, get_command_arr($data));
  0 == $? || die "\nfailed to push data to Emacs";
}

sub emacs_start_e_sink() {
  system($EMACSCLIENT, "--no-wait", "--eval", <<AARDVARK);
(progn (require 'e-sink) (e-sink-start "${BUFFER_TITLE}"))
AARDVARK
  0 == $? || die "\nunable to open new frame\n\nconfirm emacs server started.";
}

sub emacs_finish_e_sink() {
  system($EMACSCLIENT, "--no-wait", "--eval", <<AARDVARK);
(e-sink-finish "$BUFFER_TITLE")
AARDVARK
0 == $? || die "\nunable to send finish tag.";
}

sub main() {
  ### start non-blocking read ASAP
  my $s = IO::Select->new;
  $s->add(\*STDIN);

  $EMACSCLIENT= "emacsclient";
  $BUFFER_TITLE= ($ARGV[0]? $ARGV[0] : '');

  # This script uses emacsclient, be sure to have a running server session.
  # A server-session can be started by "M-x server-start".
  emacs_start_e_sink();

  my $temp= join('', get_command_arr(''));
  my $arg_max= ARG_MAX - length($temp) - 1; # 1 for NULL string end

  my $data;
  while ( $s->can_read() ) {
    my $line= <STDIN>;

    if ( ! $line ) {
      last;
    }

    $line= esc_chars( $line );

    if ( $data && ( length($data) + length($line) > $arg_max) ) {
      push_data_to_emacs( $data );
      $data= $line;
    } else {
      $data .= $line;
    }
  }

  $data and push_data_to_emacs( $data );
  emacs_finish_e_sink;
  0;
}


main;
