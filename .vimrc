set relativenumber
set ignorecase
set hls
syntax off

set rtp+=/home/karan/.fzf/
map ; :FZF<CR>

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'python/black'
Plugin 'klen/python-mode'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
Plugin 'tpope/vim-fugitive'
" plugin from http://vim-scripts.org/vim/scripts.html
" Plugin 'L9'
" Git plugin not hosted on GitHub
Plugin 'git://git.wincent.com/command-t.git'
" git repos on your local machine (i.e. when working on your own plugin)
" Plugin 'file:///home/gmarik/path/to/plugin'
" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
Plugin 'scrooloose/nerdcommenter'
" Install L9 and avoid a Naming conflict if you've already installed a
" different version somewhere else.
" Plugin 'ascenator/L9', {'name': 'newL9'}
Plugin 'itchyny/lightline.vim'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tpope/vim-surround'
Plugin 'nvie/vim-flake8'
Plugin 'alessandroyorba/arcadia'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

set laststatus=2 "For lightline
set undodir=~/.vim/undodir
set undofile

" Set the vim leader key
:let mapleader = ","
:let maplocalleader = "\\"

set tabstop=4

" Key mappings
:nnoremap <leader>- ddo<esc>P
:inoremap <c-d> <esc>ddi
:inoremap <c-u> <esc>viwUea
:inoremap <c-'> <esc>vawS"ea
:inoremap <c-e> <esc>A
:nnoremap <c-u> viwU<esc>
:nnoremap <leader>ev :vsplit $MYVIMRC<CR>
:nnoremap <leader>sv :source $MYVIMRC<CR>
:iabbrev adn and
:iabbrev slef self
:iabbrev improt import
:iabbrev pyhton python
:iabbrev @@ karandewan@gmail.com
:vnoremap <leader>/ :call NERDComment(0, "toggle")<CR>
:nnoremap <leader>/ :call NERDComment(0, "toggle")<CR>

let g:arcadia_Midnight = 1
:colorscheme arcadia

" run black on python files
autocmd BufWritePre *.py execute ':Black'
