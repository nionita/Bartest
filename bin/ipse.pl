#!/usr/bin/env perl
use strict;
use warnings;

my $linemax = 30;
my $lineno = $linemax;

my @flds2labels = (
	[ "CPU0_TEMP" => "CPU0" ],
	[ "CPU1_TEMP" => "CPU1" ],
	[ "DIMM0 Outlet" => "DIMM0" ],
	[ "DIMM1 Inlet" => "DIMM1" ],
	[ "SR5670 Case" => "SR5670" ],
	[ "CPU0_FAN" => "Fan0" ],
	[ "CPU1_FAN" => "Fan1" ],
	[ "REAR_FAN2" => "Fan" ],
    );

my @flds   = map { $_->[0] } @flds2labels;
my @labels = map { $_->[1] } @flds2labels;

my %flds2labels = map { $_->[0], $_->[1] } @flds2labels;

sub get_sensors {
	my %vals;
	open(IN, "ipmi-sensors |") || die "Cannot open pipe: $!";
	while (my $line = <IN>) {
		chomp $line;
		my @fields = split /\s*\|\s*/, $line;
		$vals{$fields[1]}=$fields[3];
	}
	close IN;
	if (++$lineno >= $linemax) {
		for my $a (@labels)  {
			printf "%6s ", $a;
		}
		print "\n";
		$lineno = 0;
	}
	for my $f (@flds) {
		printf "%6d ", $vals{$f};
	}
	print "\n";
}

while (1) {
	get_sensors;
	sleep(59);
}
