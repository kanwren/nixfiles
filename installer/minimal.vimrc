scriptencoding utf-8
set ffs=unix
set encoding=utf-8
filetype plugin indent on
syntax on

set hidden autoread noconfirm
set noerrorbells visualbell t_vb=
set lazyredraw
set splitbelow splitright
set number relativenumber
set list listchars=tab:>-,extends:>,precedes:<
set laststatus=2 showmode cmdheight=1 showcmd
set statusline=[%n]\ %f%<\ %m%y%h%w%r\ \ %(0x%B\ %b%)%=%p%%\ \ %(%l/%L%)%(\ \|\ %c%V%)%(\ %)
set wildmenu wildmode=longest:list,full
set virtualedit=all
set scrolloff=0
set nojoinspaces backspace=indent,eol,start whichwrap+=<,>,h,l,[,]
set nrformats=bin,hex
set cpoptions+=y
set autoindent smarttab tabstop=4 expandtab softtabstop=4 shiftwidth=4
set cinoptions+=:0L0g0j1J1
set textwidth=80 nowrap
set formatoptions=croqjln
set magic noignorecase smartcase showmatch incsearch hlsearch
set timeout timeoutlen=3000 ttimeout ttimeoutlen=0

nnoremap Q @q
xnoremap <silent> Q :normal @q<CR>
xnoremap <silent> . :normal .<CR>

colorscheme elflord

