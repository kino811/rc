" Vundle
set nocompatible "required
filetype off "required
set rtp+=~/.vim/bundle/Vundle.vim "required
call vundle#begin() "required

Plugin 'VundleVim/Vundle.vim' "required
Plugin 'altercation/vim-colors-solarized'
Plugin 'bling/vim-airline'
Plugin 'tpope/vim-fugitive'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'
Plugin 'tpope/vim-surround'
Plugin 'kien/ctrlp.vim'
Plugin 'easymotion/vim-easymotion'

call vundle#end() "required
filetype plugin indent on "required


" set leader
let mapleader = ","

" file encoding
set fileencodings=utf8,euc-kr
set fileformats=dos,unix

" colorscheme
syntax enable
set background=dark
colorscheme solarized

" hide toolbar
set guioptions-=T

set number
set autoindent cindent
set tabstop=4 shiftwidth=4
set hlsearch

" syntastic setting
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_loc_list=1
let g:syntastic_check_on_open=1
let g:syntastic_check_on_wq=0

" ctrlP setting
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
set wildignore+=*.swp
let g:ctrlp_custom_ignore = '\.(git|svn)'


" easy-motion setting
map <Leader> <Plug>(easymotion-prefix)


" nerd-tree
map <C-n> :NERDTreeToggle<CR>
