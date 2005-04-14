#!/usr/bin/perl

# This dumps out 100 sentences as a single text block. It's useful for times
# when you need to cut-and-paste text into specific areas of a document.

use strict;
use warnings;
use Text::Wrap;
use Lingua::ManagementSpeak;

$Text::Wrap::columns = 78;
my $ms = new Lingua::ManagementSpeak;

print wrap('', '', $ms->paragraph(100));
