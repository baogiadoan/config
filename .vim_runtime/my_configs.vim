""Setting for carbonized colorscheme

"colorscheme carbonized-dark
"colorscheme badwolf
colorscheme gruvbox

""Setting for carbonized lightline
let g:lightline = {
    \ 'colorscheme' : 'gruvbox',
    \ }

"Setting for YouCompleteMe
let g:ycm_autoclose_preview_window_after_completion=1
map <leader>d  :YcmCompleter GoToDefinitionElseDeclaration<CR>

"Set number on vim 
set nu

"Set clipboard copy
set clipboard=unnamedplus

"Set tmux_vim seamless transfer
let g:tmux_navigator_no_mappings = 1

nnoremap <silent> <c-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <c-j> :TmuxNavigateDown<cr>
nnoremap <silent> <c-k> :TmuxNavigateUp<cr>
nnoremap <silent> <c-l> :TmuxNavigateRight<cr>
nnoremap <silent> <c-\> :TmuxNavigatePrevious<cr>

"Setting for vimux
"Run command inside VIM
map <Leader>vp :VimuxPromptCommand<CR>
"RUn last command inside VIM
map <Leader>vl :VimuxRunLastCommand<CR>

"Setting for vim-cellmode
let g:cellmode_tmux_sessionname='ipython'  " Will try to automatically pickup tmux session
let g:cellmode_tmux_windowname='ipython'
let g:cellmode_tmux_panenumber='1'


"This is where I try new vim config that I have learnt
"==================== 
set cursorline " highlight current line"

"turn off search highlight
nnoremap <leader><space> :nohlsearch<CR>
set foldlevelstart=10 " open most folds by default"
set foldnestmax=10 " 10 nested fold max"

" move vertically by visual line
nnoremap j gjzz
nnoremap k gkzz

" move to beginning/end of line 
nnoremap B ^
nnoremap E $

" $/^ doesn't do anything
nnoremap $ <nop>
nnoremap ^ <nop>

" highlight last inserted text

nnoremap gV `[v`]

" jk is escape
inoremap jk <esc>

" toogle gundo
nnoremap <leader>u :GundoToggle<CR>


" auto keep the fold in code 

augroup remember_folds
  autocmd!
  autocmd BufWinLeave * mkview
  autocmd BufWinEnter * silent! loadview
augroup END

"
""====================  

" set _( in visual mode to place the brackets on an existing codes
vnoremap _( <Esc>`>a)<Esc>`<i(<Esc>


" setup for linters and fixers
"
let g:ale_linters = {'python': ['flake8']}
let g:ale_fixers = {'*':[], 'python': ['black']}

"==================================================
"split config
" 
set splitbelow splitright
" => Open terminal inside Vim
map <Leader>tt :vnew term://zsh<CR>
" => Change 2 split windows from vert to horiz or horiz to vert
map <Leader>th <C-w>t<C-w>H
map <Leader>tk <C-w>t<C-w>K


"==================================================
"TERMINAL mode

tnoremap <Esc> <C-\><C-n>


"==================================================
"set relative
set relativenumber
