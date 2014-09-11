set tabstop=4 shiftwidth=4
set expandtab
set number
set hlsearch
set autoindent cindent

syntax enable
colorscheme slate

"abbreviation(약어)
"abbreviate(ab)
"iabbrev(ia)
abbreviate mail: kino811@gmail.com
iabbrev time: <C-R>=strftime("%Y-%m-%d %H:%M:%S")<CR>

"특정 파일에만 고유한 옵션을 지정하고 싶은경우, 파일 첫행에 다음과 같이 적으면 됨
"/* vim: set ts=2 sw=2: */
