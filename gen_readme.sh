#!/bin/sh

# README: Generate README.md
# Needs to be run in root dir of git repo.

cat README_template.md > README.md

rg '<!-- README:|# README:' * --color=never --smart-case -m 1 | sed 's/<!-- README:\|# README:\|-->\|\/README.md//g;s/^/\* \*\*/g;s/:/\*\*:/g' >> README.md
echo '\n# Author\n\nWritten by\nMeelis Utt' >> README.md
