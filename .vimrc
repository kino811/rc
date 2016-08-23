set nocompatible
set rtp+=./

" Plugins setting " {{{
" vundle {
filetype off
set rtp+=~/rc/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'pathogen.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'OmniSharp/omnisharp-vim'
Plugin 'csharp.vim'
Plugin 'taglist.vim'
Plugin 'ctrlp.vim'
Plugin 'tpope/vim-dispatch'
Plugin 'Valloric/YouCompleteMe'
Plugin 'scrooloose/syntastic'
Plugin 'shougo/unite.vim'
Plugin 'tyru/open-browser.vim'
Plugin 'majutsushi/tagbar'
Plugin 'bling/vim-airline'
Plugin 'altercation/vim-colors-solarized'
Plugin 'kshenoy/vim-signature'
Plugin 'sirver/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'scrooloose/nerdcommenter'
call vundle#end()
filetype plugin indent on
" vundle }
"

set showmatch
set langmenu=en_US
let $LANG = 'en_US'

so $VIMRUNTIME/delmenu.vim
so $VIMRUNTIME/menu.vim

let mapleader = ","
let maplocalleader = "\<space>"

set clipboard=unnamed
"set backspace=indent,eol,start
set tabstop=4 shiftwidth=4
set expandtab
set autoindent cindent
set fileencodings=ucs-bom,utf-8,korea,default
set encoding=utf-8
set ruler
set incsearch
set nowrapscan
set showcmd

set tags=./tags,tags

"set termencoding=korea
set termencoding=utf-8
set helplang=ko

set sessionoptions-=options
set sessionoptions+=unix,slash

set hlsearch
set grepprg=grep\ -n

set foldmethod=indent
set foldlevel=99

set splitbelow
set splitright
set hidden
set guioptions-=T

filetype on
filetype plugin on
filetype indent on

" set colorscheme
"colorscheme slate
if has('gui_running')
    set background=dark
    colorscheme solarized
else 
    colorscheme slate
endif

syntax enable


" UltiSnips {
let g:UltiSnipsUsePythonVersion = 2
let g:UltiSnipsSnippetsDir = '~/rc/.vim/UltiSnips'
let g:UltiSnipsExpandTrigger='<tab>'
let g:UltiSnipsJumpForwardTrigger='<tab>'
let g:UltiSnipsJumpBackwardTrigger='<s-tab>'
let g:UltiSnipsEditSplit='vertical'
" UltiSnips }

" pathogen {
execute pathogen#infect()
" pathogen }

" previm {
if has('mac')
    let g:previm_open_cmd = 'open -a Safari'
elseif has('win32')
    "let g:previm_open_cmd = 'start explorer'
endif
augroup PrevimSettings
    autocmd!
    autocmd BufNewFile,BufRead *.{md,mdwn,mkd,mkdn,mark*} set filetype=markdown
augroup END
" previm }

"
" NERDTree {
let NERDTreeIgnore=['\.pyc$', '\~$'] "ignore files in NERDTree
" NERDTree }
"
" Syntax Checking/Highlighting for python {
let python_highlight_all=1
syntax on
" Syntax Checking/Highlighting for python }
"

" YouCompleteMe {
let g:ycm_complete_in_comments = 1
let g:ycm_complete_in_strings = 1
let g:ycm_key_list_select_completion = ['<c-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<c-p>', '<Up>']
" YouCompleteMe }

" SimpyLFold {
let g:SimpylFold_docstring_preview=1
" SimpyLFold }

"  syntastic {
"to disable all style messageds
let g:syntastic_quiet_messages = {"type": "style"}
"  syntastic }

" omnisharp {{{
let g:OmniSharp_server_type = 'v1'
"let g:OmniSharp_server_type = 'roslyn'
let g:Omnisharp_host="http://localhost:2000"
let g:OmniSharp_timeout=1
let g:OmniSharp_selector_ui='ctrlp'
let g:syntastic_cs_checkers=['syntax', 'semantic', 'issues']

augroup omnisharp_commands
    autocmd!

    " Automatically add new cs files to the nearest project on save
    autocmd BufWritePost *.cs call OmniSharp#AddToProject()

    "show type information automatically when the cursor stops moving
    autocmd CursorHold *.cs call OmniSharp#TypeLookupWithoutDocumentation()

    "The following commands are contextual, based on the current cursor position.

    autocmd FileType cs nnoremap <buffer> gd :OmniSharpGotoDefinition<cr>
    autocmd FileType cs nnoremap <buffer> <localleader>fi :OmniSharpFindImplementations<cr>
    autocmd FileType cs nnoremap <buffer> <localleader>ft :OmniSharpFindType<cr>
    autocmd FileType cs nnoremap <buffer> <localleader>fs :OmniSharpFindSymbol<cr>
    autocmd FileType cs nnoremap <buffer> <localleader>fu :OmniSharpFindUsages<cr>
    autocmd FileType cs nnoremap <buffer> <localleader>fm :OmniSharpFindMembers<cr>
    autocmd FileType cs nnoremap <buffer> <localleader>x  :OmniSharpFixIssue<cr>
    autocmd FileType cs nnoremap <buffer> <localleader>fx :OmniSharpFixUsings<cr>
    autocmd FileType cs nnoremap <buffer> <localleader>tt :OmniSharpTypeLookup<cr>
    autocmd FileType cs nnoremap <buffer> <localleader>dc :OmniSharpDocumentation<cr>
    autocmd FileType cs nnoremap <buffer> <C-K> :OmniSharpNavigateUp<cr>
    autocmd FileType cs nnoremap <buffer> <C-J> :OmniSharpNavigateDown<cr>
    autocmd FileType cs nnoremap <buffer> <localleader><space> :OmniSharpGetCodeActions<cr>
    autocmd FileType cs vnoremap <buffer> <localleader><space> :call OmniSharp#GetCodeActions('visual')<cr>
    autocmd FileType cs nnoremap <buffer> <localleader>nm :OmniSharpRename<cr>
    autocmd FileType cs nnoremap <buffer> <localleader>rl :OmniSharpReloadSolution<cr>
    autocmd FileType cs nnoremap <buffer> <localleader>cf :OmniSharpCodeFormat<cr>
    autocmd FileType cs nnoremap <buffer> <localleader>tp :OmniSharpAddToProject<cr>
    autocmd FileType cs nnoremap <buffer> <localleader>ss :OmniSharpStartServer<cr>
    autocmd FileType cs nnoremap <buffer> <localleader>sp :OmniSharpStopServer<cr>
    autocmd FileType cs nnoremap <buffer> <localleader>th :OmniSharpHighlightTypes<cr>
augroup END

" omnisharp }}}

" python-mode {
let g:pymode_rope = 0
let g:pymode_doc = 1
let g:pymode_doc_key = 'K'
let g:pymode_lint = 1
let g:pymode_lint_checkers = ['pyflakes']
let g:pymode_lint_write = 1
let g:pymode_virtualenv = 1
let g:pymode_breakpoint = 1
let g:pymode_breakpoint_bind = '<leader>b'
let g:pymode_syntax = 1
let g:pymode_syntax_all = 1
let g:pymode_syntax_indent_errors = g:pymode_syntax_all
let g:pymode_syntax_space_errors = g:pymode_syntax_all
let g:pymode_folding = 0
let g:pymode_run_bind = '<leader>r'
" python-mode }
"

" 
" Plugins setting }}}


" abbreviation " {{{
iabbrev :mail: kino811@gmail.com
iabbrev :time: <C-R>=strftime("%Y-%m-%d %H:%M:%S")<CR>
" abbreviation " }}}
"

" auto command " {{{
"
autocmd FileType vim setlocal number
autocmd FileType text setlocal nonumber

augroup csharp
    autocmd!

    if has("mac")
        autocmd FileType cs map <buffer> <F5> :!mcs %:p && mono %:p:r.exe<CR>
    elseif has("win32")
        autocmd FileType cs map <buffer> <f5> :!mcs %:p && %:p:r.exe<cr>
    endif
augroup END

augroup python
    autocmd!

    au BufNewFile,BufRead *.py set tabstop=4 softtabstop=4 shiftwidth=4 textwidth=79 expandtab fileformat=unix

    if has("mac")
        autocmd FileType python map <buffer> <F5> :!python %<CR>
    elseif has("win32")
        autocmd FileType python map <buffer> <F5> :!start cmd /c "pushd %:p:h && python %:t"<CR>
    endif
augroup END

augroup lua
    autocmd!

    if has("mac")
        autocmd FileType lua map <buffer> <F5> :!lua %<CR>
    elseif has("win32")
        autocmd FileType lua map <buffer> <F5> :!start cmd /c "pushd %:p:h && lua %:t"<CR>
    endif
augroup END

augroup dosbatch
    autocmd!

    if has("win32")
        autocmd FileType dosbatch map <buffer> <F5> :!start cmd /c "pushd %:p:h && %:t"<CR>
    endif
augroup END

augroup cpp
    autocmd!

    if has('win32')
        if isdirectory("d:/dev/local/MinGW")
            autocmd FileType cpp setlocal path<
            autocmd FileType cpp setlocal path+=d:/dev/local/MinGW/mingw32/**/include/**

            autocmd FileType cpp setlocal tags<
            autocmd FileType cpp setlocal tags+=d:/dev/local/MinGW/tags
        endif
    endif
augroup END
" auto command " }}}

"
" set var " {
"todo: set gnu grep path
"let Grep_Path = ''
let Grep_OpenQuickfixWindow = 1
let Grep_Default_Options = '-rn'
" set var " }
"
" my funcs " {
function! MakeMarkdownAndPreviewFromPandoc()
    let html_name = expand('%:p:r') . '.html'
    exe '!pandoc -s -o ' . html_name . ' ' . expand('%:p')
    if has("mac")
        " todo:
        exe '!open "' . html_name . '"'
    elseif has("win32")
        exe '!start explorer "' . html_name . '"'
    endif
endfunction

function! FindReferenceAPIFromUnity3D()
    let ref_site = "http://docs.unity3d.com/ScriptReference/30_search.html?q="

    if has("mac")
        exe '!open "' . ref_site . '<cword>"'
    elseif has("win32")
        exe '!start explorer "' . ref_site . '<cword>"'
    endif
endfunction

function! FindReferenceAPIFromMSDN()
    let ref_site = 'http://www.google.com/search?hl=en&btnI=I\%27m+Feeling+Lucky&q=site\%3Amsdn.microsoft.com\%20'

    if has("mac")
        exe '!open "' . ref_site . '<cword>"'
    elseif has("win32")
        exe '!start explorer "' . ref_site . '<cword>"'
    endif
endfunction

function! TranslateWordFromEnToKr()
    let ref_site = 'https://translate.google.co.kr/?hl=ko\#en/ko/'

    if has('mac')
        exe '!open "' . ref_site . '<cword>"'
    elseif has('win32')
        exe '!start explorer "' . ref_site . '<cword>"'
    endif
endfunction

function! TranslateWordFromEnToKrThat(word)
    let ref_site = 'https://translate.google.co.kr/?hl=ko\#en/ko/'

    if has('mac')
        exe '!open "' . ref_site . a:word . '"'
    elseif has('win32')
        exe '!start explorer "' . ref_site . a:word . '"'
    endif
endfunction

function! OpenFTPServerCurrentDir(empty, ...)
    let port = 2121
    if a:0 > 0
        let port = a:1
    endif

    if has('mac')
        let fork_cmd = 'open'
    elseif has('win32')
        let fork_cmd = 'start'
    endif

    exe '!' . fork_cmd . ' python -m pyftpdlib ' . port . ''
endfunc

function! OpenHTTPServerCurrentDir(empty, ...)
    let port = 8000
    if a:0 > 0
        let port = a:1
    endif

    if has('mac')
        let fork_cmd = 'open'
    elseif has('win32')
        let fork_cmd = 'start'
    endif

    exe '!' . fork_cmd . ' python -m SimpleHTTPServer ' . port . ''
endfunction

" my funcs " }

"
" commands {
command! MakeMarkdownAndPreviewFromPandoc :call MakeMarkdownAndPreviewFromPandoc()
command! FindReferenceAPIFromUnity3D :call FindReferenceAPIFromUnity3D()
command! FindReferenceAPIFromMSDN :call FindReferenceAPIFromMSDN()
command! TranslateWordFromEnToKr :call TranslateWordFromEnToKr()
command! -nargs=1 TranslateWordFromEnToKrThat :call TranslateWordFromEnToKrThat(<f-args>)
command! SetLocalDirToThisFileDir :lcd %:p:h
command! MakeSessionToDefault :mks! ~\session.vim
command! OpenExplorerFromThisFile :!start explorer %:p:h
command! -nargs=? OpenFTPServerCurrentDir :call OpenFTPServerCurrentDir('', <f-args>)
command! -nargs=? OpenHTTPServerCurrentDir :call OpenHTTPServerCurrentDir('', <f-args>)
"
" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis
endif
" commands }

" maps " {
"about buffer
nmap <leader>bn :bnext<cr>
nmap <leader>bp :bprev<cr>
nmap <leader>bf :bfirst<cr>
nmap <leader>bl :blast<cr>
"about CtrlP
nmap <leader>cp :CtrlP<cr>
nmap <leader>cpw :CtrlPCurWD<cr>
nmap <leader>cpb :CtrlPBuffer<cr>
" about python
nnoremap <leader>16 viwy:python print(int(""", 16))<CR>
"about NerdTree
nnoremap <leader>nt :NERDTreeToggle<cr>
nnoremap <leader>ntf :NERDTreeFind<cr>

nmap <Leader>tb :TagbarToggle<CR>

nmap <leader>ts :TranslateWordFromEnToKr<cr>
nmap <leader>tsw :TranslateWordFromEnToKrThat<space>

imap <C-c> <plug>NERDCommenterInsert

nnoremap <Leader>yg :YcmCompleter GoTo<CR>
nnoremap <Leader>ygd :YcmCompleter GoToDeclaration<CR>
nnoremap <Leader>yt :YcmCompleter GetType<CR>
nnoremap <Leader>yd :YcmCompleter GetDoc<CR>
" maps " }
