""Setting for carbonized colorscheme

"colorscheme carbonized-dark
"colorscheme badwolf
"colorscheme gruvbox
syntax on
"colorscheme onedark

"base16 theme config
let base16colorspace=256
set termguicolors
colorscheme base16-chalk
"

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

"set guifont="Hack Bold Nerd Font Complete Mono" 12 
"set guifont="Hack_Bold_Nerd_Font_Complete_Mono":h12 


let g:airline_powerline_fonts = 1
let g:airline_theme='onedark'

" Undo Tree
"
nnoremap <F5> :UndotreeToggle<cr>
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

set encoding=UTF-8

"==================================================
" UltiSnip
"let g:UltiSnipsExpandTrigger="<cr>"
"
" vim-ipython
"
"------------------------------------------------------------------------------
" slime configuration 
"------------------------------------------------------------------------------
" always use tmux
let g:slime_target = 'tmux'

" fix paste issues in ipython
let g:slime_python_ipython = 1

" always send text to the top-right pane in the current tmux tab without asking
let g:slime_default_config = {
            \ 'socket_name': get(split($TMUX, ','), 0),
            \ 'target_pane': '{top-right}' }
let g:slime_dont_ask_default = 1

"------------------------------------------------------------------------------
" ipython-cell configuration
"------------------------------------------------------------------------------
" Use '##' to define cells instead of using marks
let g:ipython_cell_delimit_cells_by = 'tags'

" Keyboard mappings. <Leader> is \ (backslash) by default

" map <Leader>r to run script
nnoremap <Leader>r :IPythonCellRun<CR>

" map <Leader>R to run script and time the execution
nnoremap <Leader>R :IPythonCellRunTime<CR>

" map <Leader>c to execute the current cell
nnoremap <Leader>c :IPythonCellExecuteCell<CR>

" map <Leader>C to execute the current cell and jump to the next cell
nnoremap <Leader>C :IPythonCellExecuteCellJump<CR>

" map <Leader>l to clear IPython screen
nnoremap <Leader>l :IPythonCellClear<CR>

" map <Leader>x to close all Matplotlib figure windows
nnoremap <Leader>x :IPythonCellClose<CR>

" map [c and ]c to jump to the previous and next cell header
nnoremap [c :IPythonCellPrevCell<CR>
nnoremap ]c :IPythonCellNextCell<CR>

" map <Leader>h to send the current line or current selection to IPython
nmap <Leader>h <Plug>SlimeLineSend
xmap <Leader>h <Plug>SlimeRegionSend

" map <Leader>p to run the previous command
nnoremap <Leader>p :IPythonCellPrevCommand<CR>

" map <Leader>Q to restart ipython
nnoremap <Leader>Q :IPythonCellRestart<CR>

" map <Leader>d to start debug mode
nnoremap <Leader>d :SlimeSend1 %debug<CR>

" map <Leader>q to exit debug mode or IPython
nnoremap <Leader>q :SlimeSend1 exit<CR>


"vim-bujo
nmap <C-t> <Plug>BujoAddnormal
imap <C-t> <Plug>BujoAddinsert

nmap <C-c> <Plug>BujoChecknormal
imap <C-c> <Plug>BujoCheckinsert

