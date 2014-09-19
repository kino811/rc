" ----------------------------------------------------------------------------------
" set
set tabstop=4 shiftwidth=4
set expandtab
set autoindent cindent
set fileencodings=ucs-bom,utf-8,korea,default
set encoding=utf-8
set ruler
set incsearch
set nowrapscan
set showcmd

colorscheme slate
filetype plugin indent on

" -----------------------------------------------------------------------------------
" 조건식
"VMS 시스템에는 백업을 자동으로 하기에 백업을 켤필요 없다.
"백업 파일은 ~파일이름 식으로 저장된다.
if has("vms")
  set nobackup
else
  set backup
endif

"색을 사용할수 있는경우 문법 강조를 사용하도록 한다.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

" ------------------------------------------------------------------------------
"abbreviation(약어)
"abbreviate(ab)
"iabbrev(ia)
abbreviate mail: kino811@gmail.com
iabbrev time: <C-R>=strftime("%Y-%m-%d %H:%M:%S")<CR>

" -----------------------------------------------------------------------------
" 자동 명령

"setlocal - 현재파일에 대해서만 적용함
autocmd FileType text setlocal textwidth=78

autocmd BufRead,BufReadPost,BufNewFile Makefile
  \ set noexpandtab
  \ set nocindent

augroup filetypedetect
   au BufNewFile,BufRead *.nsh setf nsis 
augroup END

" ------------------------------------------------------------------------
" 변수 설정
"todo: set gnu grep path
"let Grep_Path = ''
let Grep_OpenQuickfixWindow = 1
let Grep_Default_Options = '-rn'

" ----------------------------------------------------------------------------
" tip

"특정 파일에만 고유한 옵션을 지정하고 싶은경우, 파일 첫행에 다음과 같이 적으면 됨
"/* vim: set ts=2 sw=2: */
