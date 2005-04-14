#!/usr/bin/perl

# This will generate a mostly nice-looking HTML file. It won't validate
# against the W3C, though.

use strict;
use warnings;
use CGI qw(header);
use Text::Wrap;
use Lingua::ManagementSpeak;

print header;
$Text::Wrap::columns = 78;
my $ms = new Lingua::ManagementSpeak;
my $document = $ms->document;

print '<html><head><title>', $document->[0]{text}, "</title>\n";

print q{
<style type="text/css">

body {
  font: 9pt Verdana, Arial, sans-serif;
  background-color: #f5f5ff;
}

p, ul {
  margin-left: 30px;
}

h1 {
  font-size: 17pt;
  margin-left: 0px;
  border-bottom: 1px black dashed;
}

h2 {
  font-size: 14pt;
  margin-left: 10px;
}

h3 {
  font-size: 12pt;
  margin-left: 20px;
}

h4 {
  font-size: 10pt;
  margin-left: 25px;
}

</style>
};

print "</head><body>\n";
print wrap('', '', $ms->to_html($document));
print "</body></html>\n";
