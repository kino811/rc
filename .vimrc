" Plugins setting " {{
" vundle " {
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'pathogen.vim'
Plugin 'davidhalter/jedi-vim'
Plugin 'OmniSharp/omnisharp-vim.git'
Plugin 'Python-3.x-Standard-Library-Reference'
Plugin 'Python-2.x-Standard-Library-Reference'
Plugin 'python.vim'
Plugin 'csharp.vim'
Plugin 'lua_omni'
Plugin 'OmniCppComplete'
Plugin 'taglist.vim'
Plugin 'The-NERD-tree'
Plugin 'ctrlp.vim'
Plugin 'tpope/vim-dispatch.git'
Plugin 'Syntastic'
Plugin 'a.vim'
call vundle#end()
filetype plugin indent on
" vundle " }

" pathogen " {
execute pathogen#infect()
" pathogen " }

" omnisharp " {
filetype plugin indent on
let g:Omnisharp_host = "http://localhost:2000"
let g:OmniSharp_timeout = 1
set noshowmatch
augroup omnisharp_commands
  autocmd!
  autocmd FileType cs setlocal omnifunc=OmniSharp#Complete
  autocmd FileType cs nnoremap <leader>b :wa!<cr>:OmniSharpBuildAsync<cr>
  autocmd BufEnter,TextChanged,InsertLeave *.cs SyntasticCheck
  autocmd BufWritePost *.cs call OmniSharp#AddToProject()
  autocmd CursorHold *.cs call OmniSharp#TypeLookupWithoutDocumentation()
  autocmd FileType cs nnoremap gd :OmniSharpGotoDefinition<cr>
  autocmd FileType cs nnoremap <leader>fs :OmniSharpFindSymbol<cr>
  autocmd FileType cs nnoremap <leader>dc :OmniSharpDocumentation<cr>
augroup END
nnoremap <leader><space> :OmniSharpGetCodeActions<cr>
vnoremap <leader><space> :call OmniSharp#GetCodeActions('visual')<cr>
nnoremap <leader>ss :OmniSharpStartServer<cr>
nnoremap <leader>sp :OmniSharpStopServer<cr>
"Don't ask to save when changing buffers (i.e. when jumping to a type
"definition)
set hidden
" omnisharp " }
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
"색을 사용할수 있는경우 문법 강조를 사용하도록 한다.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif
" set highlight " }
"
" abbreviation(약어) " {
"abbreviate(ab)
"iabbrev(ia)
abbreviate mail: kino811@gmail.com
iabbrev time: <C-R>=strftime("%Y-%m-%d %H:%M:%S")<CR>
" abbreviation(약어) " }
"
" auto command " {
"setlocal - 현재파일에 대해서만 적용함
autocmd FileType text setlocal textwidth=78
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
