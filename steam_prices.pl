#!/usr/bin/env perl

use diagnostics;
use strict;
use utf8;
use warnings;

use JSON;
use LWP::Simple;
use URI::Escape;


sub urlencode {
  my %hash = @_;
  return join '&', map { join '=', map uri_escape($_), $_, $hash{$_} } keys %hash;
}

binmode STDOUT, ':utf8';
die 'You must specify item name!' unless $#ARGV + 1;

my $base_url = 'http://steamcommunity.com/market/search/render/';
my %query_args = (
  query => $ARGV[0],
  start => 0,
  count => 10
);

my $query_string = $base_url . '?' . urlencode(%query_args);
my $contents = decode_json get($query_string);
die 'Steam returned error!' unless $contents->{success};
die 'Nothing found!' unless $contents->{total_count};

my @prices = $contents->{results_html} =~ /([\d\.]+)\s+USD/g;
my @names = $contents->{results_html} =~ /item_name.+>(.*?)<\//g;
die 'Parsing failed!' unless $#prices == $#names;

for my $idx (0 .. $#prices) {
  print $names[$idx] . ' => ' . $prices[$idx] . "\n";
}
