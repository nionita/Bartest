#!/usr/bin/env perl

use strict;
use warnings;
use RRDs;

my $rrddb = "/home/nicu/rrd/x3850m2_tf";

# The names have to be exactly like they are returned by
# ipmi-sensors - this will be checked
my %sensors = (
	30 => 'Ambient Temp',
	31 => 'Fan 1 Tach',
	33 => 'Fan 2 Tach',
	32 => 'Fan 3 Tach',
	35 => 'Fan 4 Tach',
	34 => 'Fan 5 Tach',
	36 => 'Fan 6 Tach',
	236 => 'CPU 1 PECI tics',
	237 => 'CPU 2 PECI tics',
	);

# Here we write the sensors order exctly in the order in which
# they will have to be inserted in the database
my @order = ( 236, 237, 30, 31, 33, 32, 35, 34, 36 );

my $pingcmd = "ping -c 4 x3850m2";
my $ipmicmd = "ssh x3850m2 sudo ipmi-sensors --comma-separated-output --no-header-output -r ";
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
			$vals{$id} = "";
			if (exists $sensors{$id} && $sensors{$id} eq $name) {
				$vals{$id} = $val;
			} elsif (!exists $sensors{$id}) {
				print "Error: sensor $id unknown\n";
			} else {
				print "Error: sensor $id expects "
					. $sensors{$id} . " but gets $name\n";
			}
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
