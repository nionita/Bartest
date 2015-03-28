#!/usr/bin/env perl

# This script takes pgn files in which it searches for wone or remis games of
# a given player (first parameters), transforms the positions played by that
# player to fens and labels them with the preferred move (i.e. the move played
# by that player in that position)
# The positions in which a book move was played are ignored
# Same with positions where a mate was announced

use strict;

my $me = shift @ARGV;
print "Search for $me games...\n";

open(PO, ">bm-input.fen") or die "Can't create fen file: $!\n";

for my $file (@ARGV) {
	print "File $file...\n";
	my %game;
	my $have_game = 0;
	my $my_col = 0;		# 0 - none, 1 - white, -1 - black
	open(IF, $file) or die "Can't open file $file: $!\n";
	while (my $line = <IF>) {
		chomp $line;
		if ($line =~ /^\[Event/) {
			if ($have_game) {
				game_to_fen(\%game, $my_col) if $my_col && my_game(\%game, $my_col);
				%game = ();
				$my_col = 0;
				# exit 0;	# debug
			}
			$have_game = 1;
			push @{$game{hea}}, $line;
		} elsif ($line =~ /^\[Round "(.+)"\]/) {
			$game{rnd} = $1;
			push @{$game{hea}}, $line;
		} elsif ($line =~ /^\[White "(.+)"\]/) {
			print $1;
			++$my_col if $1 =~ /^$me/;
			push @{$game{hea}}, $line;
		} elsif ($line =~ /^\[Black "(.+)"\]/) {
			print " - ", $1;
			--$my_col if $1 =~ /^$me/;
			push @{$game{hea}}, $line;
		} elsif ($line =~ /^\[Result "(.+)"\]/) {
			print " : ", $1, "\n";
			$game{res} = $1;
			push @{$game{hea}}, $line;
		} elsif ($line =~ /^\[PlyCount "(\d+)"\]/) {
			$game{ply} = $1;
			push @{$game{hea}}, $line;
		} elsif ($line =~ /^\[%/) {	# this is an [%emt inside a comment
			$game{mvs} .= " $line";
		} elsif ($line =~ /^\[/) {
			push @{$game{hea}}, $line;
		} else {
			$game{mvs} .= " $line";
		}
	}
	game_to_fen(\%game, $my_col) if $have_game && $my_col && my_game(\%game, $my_col);
	close IF;
}
close PO;

# Is this game wone or remis?
sub my_game {
	my ($game, $my_col) = @_;
	if ($my_col > 0) {
		if ($game->{res} eq "0-1") { return 0; } else { return 1; }
	} else {
		if ($game->{res} eq "1-0") { return 0; } else { return 1; }
	}
}

# Indicates the comment a book move?
sub is_book {
	my $com = shift;
	return (undef $com || $com eq "" || $com eq "{book}" || $com eq "{0s}" || $com =~ /^\{\+0\.00\/0\s/);
}

# Handle one game
sub game_to_fen {
	my ($game, $my_col) = @_;
	my @mvs;
	my $mvs = $game->{mvs};
	my $mate = 0;
	my $book = 0;
	# First preprocess the lines of the game
	$mvs =~ s/\r//gs;
	@mvs = split /\d+\.\s+/, $mvs; shift @mvs;
#	while (!$mate and $mvs =~ /^\s+(\d+\.)\s+(.+?)\s+({.+?})?\s+(.+?)\s+({.+?})?(.*)/s) {
#		my ($movno, $wmove, $wcom, $bmove, $bcom) = ($1, $2, $3, $4, $5);
#		print $movno, ":", $wmove, ":", $wcom, ":", $bmove, ":", $bcom, "\n";
#		$mvs = $6;
#		$book++ if is_book($wcom);
#		$book++ if is_book($bcom);
#		if ($wcom !~ /^{[+-]M/) {
#			if ($bcom =~ /^{[+-]M/) {
#				push @mvs, "$movno $wmove";
#				$mate = 1;
#			} else {
#				push @mvs, "$movno $wmove $bmove"
#			}
#		} else {
#			$mate = 1;
#		}
#	}
#	if (!$mate and $mvs =~ /^\s+(\d+\.)\s+(.+?)\s+({.+?})?(.*)/s) {
#		my ($movno, $wmove, $wcom) = ($1, $2, $3);
#		if ($wcom !~ /^{[+-]M/) {
#			push @mvs, "$movno $wmove";
#		}
#	}
#	push @mvs, $game->{res};
	
	# Now write a tmp game file
	open(OF, ">temp.pgn") or die "Can't create temp pgn: $!\n";
	foreach my $line (@{$game->{hea}}) {
		$line =~ s/\r//g;
		print OF $line, "\n";
		#print $line, "\n";
	}
	print OF "\n";
	my $movno = 0;
	foreach my $line (@mvs) {
		++$movno;
		print OF $movno, ". ", $line, "\n";
	}
	close OF;
	#print $game->{res}, "\n";
	
	# Now run pgntofen:
	open(PI, "pgn2fen.exe temp.pgn 2>/dev/null |") or die "Can't open pipe to pgn2fen: $!\n";
	my ($cply, $imove, $idx) = (0, 0, 0);
	$imove = 1 if $my_col < 0;
	my $maxply = $game->{ply};
	while (my $line = <PI>) {
		chomp $line; $line =~ s/\r//;
		$idx = int($cply / 2);
		$cply++;
		$imove = 1 - $imove;
		# Skip the book moves
#		if ($cply > $book && $imove && $cply <= $maxply) {
		if ($imove && $cply <= $maxply) {
			#print $idx, ": ";
			#print $line, " --> ";
			my $mvline = $mvs[$idx];
			#print $mvline, " --> ";
			$mvline =~ s/\s{.+?}//g;
			#print $mvline, "\n";
			my @wb = split /\s/, $mvline;
			my $ix = $my_col > 0 ? 0 : 1;
			print PO $wb[$ix], "\t", $line, "\t# round ", $game->{rnd}, " ply ", $cply, "\n";
		}
	}
	close PI;
}