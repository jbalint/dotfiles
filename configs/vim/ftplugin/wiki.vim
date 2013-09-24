:if !exists("*Createwikipage")
:function! Createwikipage()
:	silent exe "file" s:createwikifile
:	call append(0,"=" . s:createwikiname . "=")
:	call append(1,"Define " . s:createwikiname . " here")
:endfunction
:endif


:if !exists("*Openwiki")
:function! Openwiki()
:	let wword = expand("<cword>")
:	if wword =~# '\u\w*[a-z0-9]\w*\u'
:		let wikidir  = fnamemodify(bufname("%"),':p:h')
:		let wikifile = wikidir . "/" . wword . ".wiki"
:		if ! filereadable(wikifile)
:			let s:createwikifile = wikifile
:			let s:createwikiname = wword
:			silent execute "tag" wword
:		else
:			silent execute "tag" wword
:		endif
:	else
:		echohl Error
:		echo '"'.wword.'"' "is not a WikiWord"
:		echohl None
:	endif
:endfunction
:endif

:if !exists("*Makewikitags")
:function! Makewikitags()
:	let s:createwikidir  = fnamemodify(bufname("%"),':p:h')
:	let tagsfile = s:createwikidir . '/' . "tags"
:	let wikifiles = s:createwikidir . '/' . "*.wiki"
:	let tagcmd = "!$HOME/.vim/tools/wikedtags.pl " . wikifiles . " > " . tagsfile
:	silent exec tagcmd
:endfunction
:endif

:autocmd! BufRead $HOME/.vim/tools/unknown.wiki call Createwikipage()
:autocmd! BufWritePost *.wiki call Makewikitags()

:map <buffer>  :call Openwiki()<CR>

