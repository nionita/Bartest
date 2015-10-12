#!/usr/bin/env perl

use strict;
use warnings;
use RRDs;

my $rrddb = "/home/nicu/rrd/dx360_tf";

# The names have to be exactly like they are returned by
# ipmi-sensors - this will be checked
my %sensors = (
	34 => 'Fan 1 Tach',
	35 => 'Fan 2 Tach',
	36 => 'Fan 3 Tach',
	37 => 'Fan 4 Tach',
	42 => 'Domain A FP Temp',
	43 => 'Domain A Temp1',
	44 => 'Domain A Temp2',
	45 => 'Domain B Temp1',
	46 => 'Domain B Temp2',
	47 => 'Domain B FP Temp',
	);

# Here we write the sensors order exctly in the order in which
# they will have to be inserted in the database
my @order = ( 43, 44, 45, 46, 42, 47, 34, 35, 36, 37 );

my $pingcmd = "ping -c 4 dx360a";
my $ipmicmd = "ssh dx360a sudo ipmi-sensors --comma-separated-output --no-header-output -r ";
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
