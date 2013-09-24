" Vim syntax file
" Language: Wiki (original non-legacy dialect)


" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif


syn match wikiWord /\w*\u\w*[a-z0-9]\w*\u\w*/
syn match wikiURL /\(http\|https\|gopher\|mailto\|news\|ftp\|file\):\(\S\+\)/
syn match wikiRule /-\{4,}/
syn match wikiBullet /^*\{1,4}\s/
syn match wikiNumItem /\(^#\{1,4}\s\)\|\(^\d\+[.)]\?\s\)/


" syn region wikiHead start="^=\{1,4}" end="=\{1,4}$" oneline
" syn region wikiItem start="^*\{1,4}" end="$" contains=ALL oneline keepend
" syn region wikiIndent start="^:\{1,4}" end="$" contains=ALL 
" syn region wikiDef start="^;" end="$" contains=ALL
" syn region wikiPre start="<pre>" end="</pre>"
" syn region wikiLink start="\[\{1,2}" end="\]\{1,2}"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version <= 508 || !exists("did_wiki_syntax_inits")
  if version < 508
    let did_wiki_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  " HiLink wikiHead               Function
  " HiLink wikiItem               Label 
  " HiLink wikiIndent		Number
  " HiLink wikiDef                Special
  " HiLink wikiPre                PreProc
  " HiLink wikiLink               Identifier 
	HiLink wikiWord		Underlined
	HiLink wikiURL		PreProc
	HiLink wikiBullet	Label
	HiLink wikiRule		Identifier
	HiLink wikiNumItem	Label

  delcommand HiLink
endif

let b:current_syntax = "wiki"

