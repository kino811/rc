" Plugins setting " {{
" vundle {
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'pathogen.vim'
Plugin 'OmniSharp/omnisharp-vim'
Plugin 'csharp.vim'
Plugin 'lua_omni'
Plugin 'OmniCppComplete'
Plugin 'taglist.vim'
Plugin 'ctrlp.vim'
Plugin 'tpope/vim-dispatch'
Plugin 'Syntastic'
Plugin 'a.vim'
Plugin 'xmledit'
Plugin 'mattn/webapi-vim'
Plugin 'tpope/vim-fugitive'
Plugin 'scrooloose/nerdtree'
Plugin 'klen/python-mode'
Plugin 'aklt/plantuml-syntax'
Plugin 'TeTrIs.vim'
Plugin 'Visual-Mark'
Plugin 'Mark'
Plugin 'kshenoy/vim-signature'
Plugin 'lua.vim'
Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-lua-ftplugin'
Plugin 'SimpylFold'
Plugin 'indentpython.vim'
Plugin 'Valloric/YouCompleteMe'
Plugin 'scrooloose/syntastic'
Plugin 'nvie/vim-flake8'
Plugin 'bling/vim-airline'
Plugin 'shougo/unite.vim'
Plugin 'kannokanno/previm'
Plugin 'tyru/open-browser.vim'
call vundle#end()
filetype plugin indent on
" vundle }
"

set langmenu=en_US
let $LANG = 'en_US'

so $VIMRUNTIME/delmenu.vim
so $VIMRUNTIME/menu.vim

let mapleader = ","

set clipboard=unnamed
set nocompatible
set backspace=start
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
set tags+=D:/project/Project_AD/repository/trunk/Real-work/commonLib/tags
set tags+=D:/dev/lib/corelib/protobuf-2.3.0/tags
set tags+=D:/dev/lib/G3DX9/trunk/tags

"set termencoding=korea
set termencoding=utf-8
set helplang=ko

set sessionoptions-=options
set sessionoptions+=unix,slash

set hlsearch
set grepprg=grep\ -n

set foldmethod=indent
set foldlevel=99

colorscheme slate

filetype on
filetype plugin on
filetype indent on

syntax enable

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
let g:ycm_global_ycm_extra_conf='~/.vim/.ycm_extra_conf.py'
let g:ycm_autoclose_preview_window_after_completion=1
let g:ycm_key_list_select_completion = ['<C-j>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-k>', '<Up>']
let g:ycm_python_binary_path='c:\usr\bin\python.exe'
nnoremap <Leader>g :YcmCompleter GoTo<CR>
nnoremap <Leader>d :YcmCompleter GoToDeclaration<CR>
nnoremap <Leader>t :YcmCompleter GetType<CR>
nnoremap <Leader>p :YcmCompleter GetParent<CR>
nnoremap <Leader>k :YcmCompleter GetDoc<CR>
" YouCompleteMe }
" 
" Virtualenv Support for python {
" python with virtualenv support
py << EOF
import os
import sys
if 'VIRTUAL_ENV' in os.environ:
    project_base_dir = os.environ['VIRTUAL_ENV']
    activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
    execfile(activate_this, dict(__file__=activate_this))
EOF
" Virtualenv Support for python }

" SimpyLFold {
let g:SimpylFold_docstring_preview=1
" SimpyLFold }

" pathogen {
execute pathogen#infect()
" pathogen }

" omnisharp {
filetype plugin on
" Selection server type. 
" v1, roslyn
let g:OmniSharp_server_type = 'v1'
" This is the default value, setting it isn't actually necessary
let g:Omnisharp_host="http://localhost:2000"
" Timeout in seconds to wait for a response from the server
let g:OmniSharp_timeout=1
" Showmatch significantly slows down omnicomplete
" when the first match contains parentheses.
set noshowmatch
" Dont't autoselect first item in omnicaomplete, show if only one item (for
" preview).
" Remove preview if you don't want to see any documentation whatsoever.
set completeopt=longest,menuone,preview
" Move the preview window (code documentation) to the bottom of the screen, so
" it doesn't move the code!
" You might also want to look at the echodoc plugin.
set splitbelow
" Get code issues and syntax errors.
let g:syntastic_cs_checkers=['syntax', 'semantic', 'issues']
" If you are using the omnisharp-roslyn backend, use the following.
let g:syntastic_cs_checkers=['code_checker']

augroup omnisharp_commands
  autocmd!
  " Set autocomplete function to OmniSharp (if not using YouCompleteMe
  " completion plugin)
  autocmd FileType cs setlocal omnifunc=OmniSharp#Complete

  " Synchonous build (bocks Vim)
  "autocmd FileType cs nnoremap <F5> :wa!<cr>:OmniSharpBuild<cr>
  
  " Build can also run asynchronously with vim-dispatch installed
  autocmd FileType cs nnoremap <leader>b :wa!<cr>:OmniSharpBuildAsync<cr>
  " Automatic syntax check on events (TextChanged requires Vim 7.4)
  autocmd BufEnter,TextChanged,InsertLeave *.cs SyntasticCheck
  " Automatically add new cs files to the nearest project on save
  autocmd BufWritePost *.cs call OmniSharp#AddToProject()
  " Show type information automatically when the cursor stops moving
  autocmd CursorHold *.cs call OmniSharp#TypeLookupWithoutDocumentation()
  
  " The following commands are contextual, based on the current cursor
  " position.
  autocmd FileType cs nnoremap gd :OmniSharpGotoDefinition<cr>
  autocmd FileType cs nnoremap <leader>if :OmniSharpFindImplementations<cr>
  autocmd FileType cs nnoremap <leader>ft :OmniSharpFindType<cr>
  autocmd FileType cs nnoremap <leader>fs :OmniSharpFindSymbol<cr>
  autocmd FileType cs nnoremap <leader>fu :OmniSharpFindUsages<cr>
  " Finds members in the current buffer
  autocmd FileType cs nnoremap <leader>fm :OmniSharpFindMembers<cr>
  " Cursor can be anywhere on the line containing an issue.
  autocmd FileType cs nnoremap <leader>x :OmniSharpFixIssue<cr>
  autocmd FileType cs nnoremap <leader>fx :OmniSharpFixUsings<cr>
  autocmd FileType cs nnoremap <leader>tt :OmniSharpTypeLookup<cr>
  autocmd FileType cs nnoremap <leader>dc :OmniSharpDocumentation<cr>
  " Navigate up by method/property/field
  autocmd FileType cs nnoremap <C-K> :OmniSharpNavigateUp<cr>
  " Navigate down by method/property/field
  autocmd FileType cs nnoremap <C-J> :OmniSharpNavigateDown<cr>
augroup END

" This setting controls how long to wait (in ms) before fetching type / symbol
" information.
"set updatetime=500

" Remove 'Press Enter to continue' message when type information is longer
" than one line.
"set cmdheight=2

" Contextual code actions (requires CtrlP or unite.vim).
nnoremap <leader><space> :OmniSharpGetCodeActions<cr>
" Run code actions with text selected in visual mode to extract method.
vnoremap <leader><space> :call OmniSharp#GetCodeActions('visual')<cr>
" Remane with dialog
nnoremap <leader>nm :OmniSharpRename<cr>
"nnoremap <F2> :OmniSharpRename<cr>
" Rename without dialog - with cursor on the symbol to rename... ':Rename
" newname'
command! -nargs=1 Rename :call OmniSharp#RenameTo("<args>")

" Force OmniSharp to reload the solution. Useful when switching branches etc.
nnoremap <leader>rl :OmniSharpReloadSolution<cr>
nnoremap <leader>cf :OmniSharpCodeFormat<cr>
" Load the current .cs file to the nearest project
nnoremap <leader>tp :OmniSharpAddToProject<cr>
" (Expreimental - uses vim-dispatch or vimproc plugin) - Start the omnisharp
" server for the current solution.
nnoremap <leader>ss :OmniSharpStartServer<cr>
nnoremap <leader>sp :OmniSharpStopServer<cr>
" Add syntax highlighting for types and interfaces.
nnoremap <leader>th :OmniSharpHighlightTypes<cr>
"Don't ask to save when changing buffers (i.e. when jumping to a type
"definition)
set hidden
" omnisharp }

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
" Plugins setting }}


" abbreviation " {
"abbreviate(ab)
"iabbrev(ia)
abbreviate mail: kino811@gmail.com
iabbrev time: <C-R>=strftime("%Y-%m-%d %H:%M:%S")<CR>
" abbreviation " }
"
" auto command " {
"setlocal - apply current file only.
autocmd BufRead,BufReadPost,BufNewFile Makefile  set noexpandtab 
autocmd BufRead,BufReadPost,BufNewFile Makefile  set nocindent
"au BufNewFile,BufRead *.py,*.pyw,*.c,*.h,*.cpp match BadWhitespace /\s\+$/

augroup python
    autocmd!
    autocmd FIleType python setlocal number
    au BufNewFile,BufRead *.py set tabstop=4 softtabstop=4 shiftwidth=4 textwidth=79 expandtab autoindent fileformat=unix

    if has("mac")
        autocmd BufEnter *.py map <F5> :!python %<CR>
    elseif has("win32")
        autocmd BufEnter *.py map <F5> :!start cmd /c "pushd %:p:h && python %:t"<CR>
    endif
augroup END

augroup filetypedetect
    autocmd BufNewFile,BufRead *.nsh setf nsis 
augroup END

augroup lua
    autocmd!

    if has("mac")
        autocmd BufEnter *.lua map <F5> :!lua %<CR>
    elseif has("win32")
        autocmd BufEnter *.lua map <F5> :!start cmd /c "pushd %:p:h && lua %:t"<CR>
    endif
augroup END

augroup dosbatch
    autocmd!

    if has("win32")
        autocmd BufEnter *.bat map <F5> :!start cmd /c "pushd %:p:h && %:t"<CR>
    endif
augroup END

" auto command " }
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
    let s:html_name = expand('%:p:r') . '.html'
    exe '!pandoc -s -o ' . s:html_name . ' ' . expand('%:p')
    if has("mac")
        " todo:
        exe '!open "' . s:html_name . '"'
    elseif has("win32")
        exe '!start explorer "' . s:html_name . '"'
    endif
endfunction

function! FindReferenceAPIFromUnity3D()
    let s:ref_site = "http://docs.unity3d.com/ScriptReference/30_search.html?q="

    if has("mac")
        exe '!open "' . s:ref_site . '<cword>"'
    elseif has("win32")
        exe '!start explorer "' . s:ref_site . '<cword>"'
    endif
endfunction

function! FindReferenceAPIFromMSDN()
    let s:ref_site = 'http://www.google.com/search?hl=en&btnI=I\%27m+Feeling+Lucky&q=site\%3Amsdn.microsoft.com\%20'

    if has("mac")
        exe '!open "' . s:ref_site . '<cword>"'
    elseif has("win32")
        exe '!start explorer "' . s:ref_site . '<cword>"'
    endif
endfunction

function! TranslateWordFromEnToKr()
    let s:ref_site = 'https://translate.google.co.kr/?hl=ko\#en/ko/'

    if has('mac')
        exe '!open "' . s:ref_site . '<cword>"'
    elseif has('win32')
        exe '!start explorer "' . s:ref_site . '<cword>"'
    endif
endfunction

function! TranslateWordFromEnToKrThat(word)
    let s:ref_site = 'https://translate.google.co.kr/?hl=ko\#en/ko/'

    if has('mac')
        exe '!open "' . s:ref_site . a:word . '"'
    elseif has('win32')
        exe '!start explorer "' . s:ref_site . a:word . '"'
    endif
endfunction
" my funcs " }

" maps " {
map <C-Tab> :bnext<CR>
map <C-S-Tab> :bprevious<CR>
nnoremap <leader>16 viwy:python print(int(""", 16))<CR>
map <Leader><C-P> :CtrlPCurWD<CR>
nnoremap <Leader><C-F> :NERDTreeFind<CR>
" maps " }
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
"
" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif
" commands }
