
augroup filetypedetect
	"au!

	" For wiki - see http://cuba.calyx.nl/~tim/wiked/
	au BufRead,BufNewFile *.wiki			setf wiki

	" Add AspectJ as Java Type
	au BufRead,BufNewFile *.aj				setf java

	" Add Pro/C (Oracle) and PostgreSQL as ESQL Type
	au BufRead,BufNewFile *.pc				setf esqlc
	au BufRead,BufNewFile *.pgc				setf esqlc
	au BufRead,BufNewFile *.sqc				setf esqlc

	" Innovis Oracle Templating for Database fields
	au BufRead,BufNewFile *.tpc				setf esqlc
	au BufRead,BufNewFile *.tpm				setf perl
	au BufRead,BufNewFile *.tc				setf c
	au BufRead,BufNewFile *.th				setf c

	" Innovis Repeat Script Files
	au BufRead,BufNewFile *.trpc			setf esqlc
	au BufRead,BufNewFile *.rpc				setf esqlc
	au BufRead,BufNewFile *.rc				setf c
	au BufRead,BufNewFile *.trc				setf c

	" I don't know what tads is so use perl as filetype
	" for *.t (perl module tests)
	au BufRead,BufNewFile *.t				setf perl

	" Read WSDD files as XML
	au	BufRead,BufNewFile *.wsdd			setf xml

	" occam
	au BufNewFile,BufRead *.occ,*.inc		setf occam

	" Nicer formatting for XML files
	au BufRead,BufNewFile *					cal SetupXml()
	fun! SetupXml()
		if &ft == 'xml' || &ft == 'xslt'
			set isk+=-
			set isk+=:
			set fp=xmllint\ --format\ -
		endif
	endfun

augroup end

