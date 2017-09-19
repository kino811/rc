set nocompatible
set rtp+=./

" Plugins setting " {{{
" vim-plug {
call plug#begin("~/.vim/bundle/plugins")
Plug 'iCyMind/NeoSolarized'
Plug 'scrooloose/nerdtree'
Plug 'OmniSharp/omnisharp-vim'
Plug 'tpope/vim-dispatch'
Plug 'Valloric/YouCompleteMe'
Plug 'scrooloose/syntastic'
Plug 'shougo/unite.vim'
Plug 'tyru/open-browser.vim'
Plug 'majutsushi/tagbar'
Plug 'bling/vim-airline'
Plug 'kshenoy/vim-signature'
Plug 'sirver/ultisnips'
Plug 'honza/vim-snippets'
Plug 'scrooloose/nerdcommenter'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-fugitive'
Plug 'easymotion/vim-easymotion'
Plug 'kino811/KinoDic'
Plug 'pangloss/vim-javascript'
Plug 'mattn/emmet-vim'
Plug 'moll/vim-node'
call plug#end()
" vim-plug }
"

filetype plugin indent on

set number
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

set foldmethod=manual
set foldlevel=99

set splitbelow
set splitright
set hidden
set guioptions-=T

filetype on
filetype plugin on
filetype indent on

" set colorscheme
set termguicolors
colorscheme NeoSolarized
set background=dark

syntax enable

" solarized {{{
let g:solarized_italic = 0
" solarized }}}

" UltiSnips {
let g:UltiSnipsUsePythonVersion = 2
let g:UltiSnipsSnippetsDir = '~/rc/.vim/UltiSnips'
let g:UltiSnipsExpandTrigger='<tab>'
let g:UltiSnipsJumpForwardTrigger='<tab>'
let g:UltiSnipsJumpBackwardTrigger='<s-tab>'
let g:UltiSnipsEditSplit='vertical'
" UltiSnips }

" previm {
if has('mac')
    let g:previm_open_cmd = 'open -a Safari'
elseif has('win32')
    "let g:previm_open_cmd = 'start explorer'
endif
augroup PrevimSettings
    autocmd!
    autocmd BufNewFile,BufRead *.{md,mdwn,mkd,mkdn,mark*} set filetype=markdown

    "autocmd BufWritePost *.md call MakeMarkdownAndPreviewFromPandoc()
    autocmd FileType markdown map <buffer> <F5> :call MakeMarkdownAndPreviewFromPandoc()<cr>
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
let g:ycm_key_list_select_completion = ['<c-j>', '<Down>']
let g:ycm_key_list_previous_completion = ['<c-k>', '<Up>']
let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'
" YouCompleteMe }

" SimpyLFold {
let g:SimpylFold_docstring_preview=1
" SimpyLFold }

"  syntastic {
"to disable all style messageds
let g:syntastic_quiet_messages = {"type": "style"}
"  syntastic }

" omnisharp {{{
filetype plugin on
let g:OmniSharp_server_type = 'v1'
let g:OmniSharp_port = 2000
let g:OmniSharp_timeout = 1
set noshowmatch
set completeopt=longest,menuone,preview
set splitbelow
let g:syntastic_cs_checkers = ['syntax', 'semantic', 'issues']
let g:OmniSharp_selector_ui = 'ctrlp'
"let g:OmniSharp_server_config_name = ""

augroup omnisharp
    autocmd!

    autocmd FileType cs setlocal omnifunc=OmniSharp#Complete
    "autocmd BufEnter,TextChanged,InsertLeave *.cs SyntasticCheck
    autocmd BufWritePost *.cs call OmniSharp#AddToProject()
    autocmd CursorHold *.cs call OmniSharp#TypeLookupWithoutDocumentation()

    autocmd FileType cs nnoremap <buffer> <localleader>gd :OmniSharpGotoDefinition<cr>
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
    
    "autocmd FileType cs nnoremap <buffer> <localleader>ss :OmniSharpStartServer<cr>
    autocmd FileType cs nnoremap <buffer> <localleader>ss :call OmniSharpStartServerBySync()<cr>

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

function! OmniSharpStartServerBySync()
    let omnisharpServerIsRunning = OmniSharp#ServerIsRunning()
    if !omnisharpServerIsRunning 
        if has("mac")
            exe "!open mono " . g:OmniSharp_server_path . " -p " . g:OmniSharp_port . " -s " . g:OmniSharp_running_slns[0]
        elseif has("win32")
            exe "!start " . g:OmniSharp_server_path . " -p " . g:OmniSharp_port . " -s " . g:OmniSharp_running_slns[0]
        endif
    else
        echo 'omnisharpServerIsRunning'
    endif
endfunc
" my funcs " }

"
" commands {
command! MakeMarkdownAndPreviewFromPandoc :call MakeMarkdownAndPreviewFromPandoc()
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
nmap <leader>cpf :CtrlPCurFile<cr>
" about python
nnoremap <leader>16 viwy:python print(int(""", 16))<CR>
"about NerdTree
nnoremap <leader>nt :NERDTreeToggle<cr>
nnoremap <leader>ntf :NERDTreeFind<cr>

nmap <Leader>tb :TagbarToggle<CR>
nmap <Leader>tbj :TagbarOpen j<CR>

nmap <leader>ts :TranslateWordFromEnToKr<cr>
nmap <leader>tsw :TranslateWordFromEnToKrThat<space>

nnoremap <Leader>yg :YcmCompleter GoTo<CR>
nnoremap <Leader>yd :YcmCompleter GoToDeclaration<CR>
nnoremap <Leader>yt :YcmCompleter GetType<CR>
nnoremap <Leader>yk :YcmCompleter GetDoc<CR>

nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader>gps :Git push<cr>
nnoremap <leader>gpu :Git pull<cr>

" easymotion
nmap <leader><leader>s <plug>(easymotion-overwin-f)
map <leader><leader>j <plug>(easymotion-j)
map <leader><leader>k <plug>(easymotion-k)

cmap <c-x><c-o> <c-r>=expand("%")<cr>

nmap <leader>lcd :SetLocalDirToThisFileDir<cr>

if has("nvim")
    :tnoremap <C-w><C-h> <C-\><C-n><C-w><C-h>
    :tnoremap <C-w><C-j> <C-\><C-n><C-w><C-j>
    :tnoremap <C-w><C-k> <C-\><C-n><C-w><C-k>
    :tnoremap <C-w><C-l> <C-\><C-n><C-w><C-l>
endif
" maps " }
