" Plugins setting " {{
" vundle " {
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'pathogen.vim'
Plugin 'davidhalter/jedi-vim'
Plugin 'OmniSharp/omnisharp-vim'
Plugin 'Python-3.x-Standard-Library-Reference'
Plugin 'Python-2.x-Standard-Library-Reference'
Plugin 'python.vim'
Plugin 'csharp.vim'
Plugin 'lua_omni'
Plugin 'OmniCppComplete'
Plugin 'taglist.vim'
Plugin 'ctrlp.vim'
Plugin 'tpope/vim-dispatch'
Plugin 'Syntastic'
Plugin 'a.vim'
Plugin 'xmledit'
Plugin 'bling/vim-airline'
Plugin 'vim-pandoc/vim-pandoc'
Plugin 'vim-pandoc/vim-pandoc-syntax'
Plugin 'mattn/webapi-vim'
call vundle#end()
filetype plugin indent on
" vundle " }

" pathogen " {
execute pathogen#infect()
" pathogen " }

" omnisharp " {
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
nnoremap <F2> :OmniSharpRename<cr>
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
" omnisharp " }

" 
" Plugins setting " }}

set tabstop=4 shiftwidth=4
set expandtab
set autoindent cindent
set fileencodings=ucs-bom,utf-8,korea,default
set encoding=utf-8
set ruler
set incsearch
set nowrapscan
set showcmd
set tags+=./tags

colorscheme slate
filetype plugin indent on
syntax on

" set highlight " {
" use syntax-highlighting when usable color.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif
" set highlight " }
"
" abbreviation " {
"abbreviate(ab)
"iabbrev(ia)
abbreviate mail: kino811@gmail.com
iabbrev time: <C-R>=strftime("%Y-%m-%d %H:%M:%S")<CR>
" abbreviation " }
"
" auto command " {
"setlocal - apply current file only.
autocmd BufRead,BufReadPost,BufNewFile Makefile set noexpandtab 
autocmd BufRead,BufReadPost,BufNewFile Makefile set nocindent
augroup filetypedetect
   au BufNewFile,BufRead *.nsh setf nsis 
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
function! ShowPreviewOfMarkdownFromPandoc()
    !pandoc -s -o temp.html %
    if has("mac")
        " todo:
        !open temp.html
    elseif has("win32")
        !start explorer temp.html
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
" my funcs " }

" maps " {
map <C-Tab> :bnext<Enter>
map <C-S-Tab> :bprevious<Enter>
nnoremap <leader>16 viwy:python print int(""", 16)<Enter>
" maps " }
"
" commands {
command! MarkdownPreviewFromPandoc call ShowPreviewOfMarkdownFromPandoc()
command! FindReferenceAPIFromUnity3D call FindReferenceAPIFromUnity3D()
command! FindReferenceAPIFromMSDN call FindReferenceAPIFromMSDN()
" commands }
