#!/usr/bin/env perl

## Use IO::Select to read STDIN without blocking

use strict;
use warnings;
use IO::Select;
use File::Spec;

use vars qw($EMACSCLIENT @EMACSCLIENT_ARGS $BUFFER_TITLE $TEMP_FILE $TEMP_FILE_H $DEBUG $TEE $CPOUT %SIG_NAME_TABLE $SIG_RECEIVED);

sub exit_code_from_sig_name($) {
  my $my_sig = shift;

  return 0 unless $my_sig;

  unless (%SIG_NAME_TABLE) {
    use Config qw(%Config);
    defined $Config{sig_name} || die "No sigs?";
    my $i= 0;
    foreach my $name (split(' ', $Config{sig_name})) {
      $SIG_NAME_TABLE{$name} = $i;
      $i++;
    }
  }

  $SIG_NAME_TABLE{$my_sig}? $SIG_NAME_TABLE{$my_sig} + 128 : 128;
}

sub system_no_stdout(\@) {
  my ($params) = @_;
  open($CPOUT, ">&", "STDOUT");
  open(STDOUT, '>', File::Spec->devnull());
  my $ret_val;
  if (system @$params) {
    die "\n system call with parameters @$params failed: \n\n$!";
  }
  open(STDOUT, ">&", $CPOUT);
  $ret_val;
}

sub get_command_arr(;\$) {
  return ($EMACSCLIENT, @EMACSCLIENT_ARGS, '--no-wait', '--eval');
}

sub push_data_to_emacs(\$) {
  my ($data) = @_;
  print $TEMP_FILE_H $$data;
}

sub emacs_start_e_sink() {
  my $elisp= qq/(e-sink-start "$BUFFER_TITLE" $TEMP_FILE)/;

  $elisp =~ s<([\"\$])><\\$1>g;
  $elisp= "\"$elisp\"";
  my @arr= (get_command_arr(), $elisp);
  my $str= join(' ', @arr);

  ## initialize $TEMP_FILE from Emacs output
  $TEMP_FILE=`$str`;
  if ($? == 0) {
    $TEMP_FILE =~ /.*"([^"]+)".*/;
    $TEMP_FILE = $1;
    print "debug \$TEMP_FILE is '$TEMP_FILE'\n" if $DEBUG;
  } else {
    die "$str returned with: $!"
  }
}

sub emacs_finish_e_sink() {
  my @arr;

  my $signalStr= $SIG_RECEIVED? "\"$SIG_RECEIVED\"" : "";

  @arr= (get_command_arr(), qq/(e-sink-finish "$BUFFER_TITLE" $signalStr)/);

  system_no_stdout(@arr);
}

sub print_help() {
  print <<AARDVARK
Usage: $0 [OPTION]... [buffer-name]

  --tee output to STDOUT as well
  -h    this screen

AARDVARK
}

sub process_args() {

  $TEMP_FILE= 1;

  for my $i ( 0..$#ARGV ) {
    if ( grep /$ARGV[$i]/, ("--help", "-h") ) {
      print_help();
      exit(0);
    } elsif ( $ARGV[$i] eq "--tee" ) {
      $TEE= 1;
      delete $ARGV[$i];
    } elsif ( $ARGV[$i] eq "--debug" ) {
      $DEBUG= 1;
    } elsif ( $ARGV[$i] =~ /\A-/ ) {
      print STDERR "unexpected option '$ARGV[$i]'\n";
      exit exit_code_from_sig_name('EXIT');
    }
  }
  @ARGV= grep(defined, @ARGV);

  if ( scalar(@ARGV) > 1) {
    print STDERR "unexpected '$ARGV[1]'\n";
    exit(1);
  }
}

sub main() {

  process_args();

  ### start non-blocking read ASAP
  my $s = IO::Select->new;
  $s->add(\*STDIN);

  $EMACSCLIENT= $ENV{EMACSCLIENT} || "emacsclient";
  @EMACSCLIENT_ARGS = $ENV{EMACSCLIENT_ARGS} ? split /\s+/, $ENV{EMACSCLIENT_ARGS} : ();
  $BUFFER_TITLE= ($ARGV[0]? $ARGV[0] : '');

  # autoflush
  select(STDOUT);
  $|= 1;

  emacs_start_e_sink();

  my $arg_max;
  my $data= '';

  $arg_max= 1;			# write file asap so Emacs can update
  open($TEMP_FILE_H, ">$TEMP_FILE");

  # disable buffering: sacrifice performance for instant gratification
  select $TEMP_FILE_H;
  $|= 1;
  select STDOUT;

  my $handler= sub {
    $SIG_RECEIVED= shift;
    $s->remove(\*STDIN);
    close(STDIN) or die "could not close STDIN";
    # if we don't reopen STDIN, we get a warning: "Filehandle STDIN reopened as
    # <> only for output." http://markmail.org/message/j76ed5ko3ouxtzl4
    open(STDIN, "<", File::Spec->devnull());
  };

  for my $s (qw(HUP INT PIPE TERM)) {
    $SIG{$s}= $handler;
  }

  while ( $s->can_read() ) {
    my $line= <STDIN>;

    if ( ! $line ) {
      last;
    }

    print $line if $TEE;

    if ( $data && ( length($data) + length($line) > $arg_max) ) {
      push_data_to_emacs( $data );
      $data= $line;
    } else {
      $data .= $line;
    }
  }

  $data and push_data_to_emacs( $data );
  $TEMP_FILE_H and close($TEMP_FILE_H);
  emacs_finish_e_sink();
  unlink $TEMP_FILE or warn "Could not unlink $TEMP_FILE";
  exit exit_code_from_sig_name($SIG_RECEIVED);
}


main;
