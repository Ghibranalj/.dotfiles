source $HOME/.config/nvim/vim-plug/plugins.vim

let g:material_theme_style = 'default'
nnoremap <M-w> :NERDTreeToggle<CR>

let NERDTreeShowHidden=1
colorscheme material

if !has('nvim')
  let &t_ZH="\e[3m"
  let &t_ZR="\e[23m"
endif

set number

