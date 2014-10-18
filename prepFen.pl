#!/usr/bin/env perl

use strict;

open(PO, ">test.fen") or die "Can't create fen file: $!\n";
open(NO, ">test.txt") or die "Can't create txt file: $!\n";

for my $file (@ARGV) {
	my %game;
	my $have_game = 0;
	open(IF, $file) or die "Can't open file $file: $!\n";
	while (my $line = <IF>) {
		chomp $line;
		if ($line =~ /^\[Event/) {
			if ($have_game) {
				game_to_fen(\%game);
				%game = ();
			}
			$have_game = 1;
			push @{$game{hea}}, $line;
		} elsif ($line =~ /^\[Result "(.+)"\]$/) {
			$game{res} = $1;
			push @{$game{hea}}, $line;
		} elsif ($line =~ /^\[PlyCount "(\d+)"\]$/) {
			$game{ply} = $1;
			push @{$game{hea}}, $line;
		} elsif ($line =~ /^\[/) {
			push @{$game{hea}}, $line;
		} else {
			$game{mvs} .= " $line";
		}
	}
	game_to_fen(\%game) if $have_game;
	close IF;
}
close PO;
close NO;

# Handle one game
sub game_to_fen {
	my $game = shift;
	my @mvs;
	my $mvs = $game->{mvs};
	my $mate = 0;
	my $book = 0;
	# First preprocess the lines of the game
	while (!$mate and $mvs =~ /^ +(\d+\.) (.+?) ({.+?}) (.+?) ({.+?})(.*)$/) {
		my ($movno, $wmove, $wcom, $bmove, $bcom) = ($1, $2, $3, $4, $5);
		$mvs = $6;
		$book++ if $wcom eq "{book}";
		$book++ if $bcom eq "{book}";
		if ($wcom !~ /^{[+-]M/) {
			if ($bcom =~ /^{[+-]M/) {
				push @mvs, "$movno $wmove";
				$mate = 1;
			} else {
				push @mvs, "$movno $wmove $bmove"
			}
		} else {
			$mate = 1;
		}
	}
	if (!$mate and $mvs =~ /^ +(\d+\.) (.+?) ({.+?})(.*)$/) {
		my ($movno, $wmove, $wcom) = ($1, $2, $3);
		if ($wcom !~ /^{[+-]M/) {
			push @mvs, "$movno $wmove";
		}
	}
	push @mvs, $game->{res};
	
	# Now write a tmp game file
	open(OF, ">temp.pgn") or die "Can't create temp pgn: $!\n";
	print OF "\n";
	foreach my $line (@{$game->{hea}}) {
		print OF $line, "\n";
	}
	foreach my $line (@mvs) {
		print OF $line, "\n";
	}
	close OF;
	
	# Now run pgntofen:
	open(PI, "pgn2fen.exe temp.pgn 2>/dev/null |") or die "Can't open pipe to pgn2fen: !$\n";
	my $cply = 0;
	my $ply = $game->{ply};
	my $sco = $game->{res};
	if ($sco eq "1-0") {
		$sco = "1.0";
	} elsif ($sco eq "0-1") {
		$sco = "0.0";
	} else {
		$sco = "0.5";
	}
	while (my $line = <PI>) {
		$cply++;
		# Skip the book moves
		if ($cply > $book) {
			print PO $line;
			print NO "$cply $ply $sco\n";
		}
	}
}