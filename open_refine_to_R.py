#!/usr/bin/env python2
import json
import sys
import os
if len(sys.argv) < 2:
 print “USAGE: ./utils/open_refine_to_R.py [edits.json] > r_file.R”
 exit(1)
json_file = sys.argv[-1]
#conversions = json.load(open(“state_clustering.json”))
conversions = json.load(open(json_file))
function_name = os.path.splitext(os.path.basename(json_file))[0]
print “%s = function(df) {“ %function_name
for conv in conversions:
  edits = conv[‘edits’]
  columnName = str(conv[‘columnName’])
  for edit in edits:
    froms = edit[‘from’]
    to = edit[‘to’]
    for source in froms:
      source = str(source)
      to = str(to)
      print “ df[df[, %s] == %s, %s] = %s” %(repr(columnName),
        repr(source), repr(columnName), repr(to))
print “ df”
print “}”
