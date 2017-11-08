" Vundle
set nocompatible "required
filetype off "required
set rtp+=~/.vim/bundle/Vundle.vim "required
call vundle#begin() "required

Plugin 'VundleVim/Vundle.vim' "required
Plugin 'altercation/vim-colors-solarized'

call vundle#end() "required
filetype plugin indent on "required


" colorscheme
syntax enable
set background=dark
colorscheme solarized
