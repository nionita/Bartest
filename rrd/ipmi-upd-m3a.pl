#!/usr/bin/env perl

use strict;
use warnings;
use RRDs;

my $rrddb = "/home/nicu/rrd/m3a_tf";

# The names have to be exactly like they are returned by
# ipmi-sensors - this will be checked
my %sensors = (
	16 => 'Fan 1 Tach',
	17 => 'Fan 2 Tach',
	18 => 'Fan 3 Tach',
	19 => 'Fan 4 Tach',
	20 => 'Domain A FP Temp',
	21 => 'Domain A Temp1',
	22 => 'Domain A Temp2',
	);

# Here we write the sensors order exctly in the order in which
# they will have to be inserted in the database
my @order = ( 21, 22, 20, 16, 17, 18, 19 );

my $pingcmd = "ping -c 4 m3a";
my $ipmicmd = "ssh m3a sudo ipmi-sensors --comma-separated-output --no-header-output -r ";
$ipmicmd .= join(',', keys %sensors);

print "Command:\n$ipmicmd\n";

while (1) {
	my %vals;

	open PING, "$pingcmd |" or die "Can't start ping: $!";
	my $ok = 1;
	while (my $line = <PING>) {
		if ($line =~ /100% packet loss/) {
			$ok = 0;
			last;
		}
	}
	close PING;

	if ($ok) {
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
	}

	sleep 55;
}
