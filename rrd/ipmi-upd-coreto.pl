#!/usr/bin/env perl

use strict;
use warnings;
use RRDs;

my $rrddb = "/home/nicu/rrd/coreto_tf";

# The names have to be exactly like they are returned by
# ipmi-sensors - this will be proofed
my %sensors = (
	2144 => 'CPU0_TEMP',
	2160 => 'CPU1_TEMP',
	2048 => 'DIMM0 Outlet',
	2064 => 'DIMM1 Inlet',
	2080 => 'SR5670 Case',
	2304 => 'CPU0_FAN',
	2320 => 'CPU1_FAN',
	2272 => 'REAR_FAN2',
	);

# Here we write the sensors order exctly in the order in which
# they will have to be inserted in the database
my @order = ( 2144, 2160, 2048, 2064, 2080, 2304, 2320, 2272 );

my $ipmicmd = "sudo ipmi-sensors --comma-separated-output --no-header-output -r ";
$ipmicmd .= join(',', keys %sensors);

print "Command:\n$ipmicmd\n";

while (1) {
	my %vals;

	open IPMI, "$ipmicmd |" or die "Can't start ipmi-sensors: $!";
	while (my $line = <IPMI>) {
		my ($id, $name, $val) = (split ",", $line)[0,1,3];
		$vals{$id} = $val if exists $sensors{$id} && $sensors{$id} eq $name;
	}
	close IPMI;

	my $uvals = "N";
	for my $id (@order) {
		$uvals .= ":" . $vals{$id} || "U";
	}

	print "Update: $uvals\n";

	RRDs::update($rrddb, $uvals);
	my $err = RRDs::error;
	die "Error while updating $rrddb: $err\n" if $err;

	sleep 59;
}
