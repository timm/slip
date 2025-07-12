#!/usr/bin/env bash

badges='
<p>
<a href="https://github.com/timm/slip/blob/main/LICENSE.md"><img 
   src="https://img.shields.io/badge/license-MIT-brightgreen.svg?xstyle=for-the-badge" 
   alt="License"></a>
<a href="https://gigamonkeys.com/book/introduction-why-lisp"><img 
   src="https://img.shields.io/badge/language-Lisp-purple.svg?xstyle=for-the-badge" 
   alt="Language"></a>
<a href="https://github.com/timm/slip"><img 
   src="https://img.shields.io/badge/src-code-orange.svg?xstyle=for-the-badge" 
   alt="Source Code"></a>
</p>
'

for f in "$1"/*.html; do
  gawk -v l="$badges" '
    sub(/<div class=.docs.><h1>/, "") {
      print "<div class=\"docs\">" l "<h1>" $0
      next
    }
    1' "$f" > "$f.tmp"
  mv "$f.tmp" "$f"
done
