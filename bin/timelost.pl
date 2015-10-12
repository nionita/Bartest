#!/usr/bin/env perl

use strict;
use warnings;

my $tourarch = "/home/nicu/Tour/archive";

my $date = $ARGV[0];

my @files;

if (!defined $date) {
	@files = glob "$tourarch/TEST-*/TEST-*.log";
} else {
	@files = glob "$tourarch/TEST-*-$date*/TEST-*.log";
}

for my $file (@files) {
	$file =~ /.*-(\d+)\.log/;
	my $dt = $1;
	open FH, $file;
	my $games = 0;
	my $touts = 0;
	while (my $line = <FH>) {
		if ($line =~ /Finished game/) {
			++$games;
			if ($line =~ / on time/) {
				++$touts;
			}
		}
	}
	close FH;
	printf "DT: %-30s %4d %3d (%5.2f)\n",
		$dt, $games, $touts, $touts*100/$games;
}
