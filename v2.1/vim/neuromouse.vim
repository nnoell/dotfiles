" Vim color file
" Maintainer:  nnoell <nnoell3[at]gmail.com>
" Last Change: 2011-09-04
" Description: Milomouse vim theme modified by nnoell with gui support (Made for 256-color console).

set background=dark
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="neuromouse"

if &t_Co > 255
  hi Normal         guifg=#5f5f5f guibg=#020202 gui=NONE term=NONE cterm=NONE ctermfg=59   ctermbg=NONE
  hi Boolean        guifg=#b94062 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=9    ctermbg=NONE
  hi Character      guifg=#b94062 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=9    ctermbg=NONE
  hi Comment        guifg=#444444 guibg=#1c1c1c gui=NONE term=NONE cterm=NONE ctermfg=238  ctermbg=234
  hi Conditional    guifg=#77b6c5 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=6    ctermbg=NONE
  hi Constant       guifg=#7E62B3 guibg=NONE	gui=NONE term=NONE cterm=NONE ctermfg=13   ctermbg=NONE
  hi Cursor         guifg=#121212 guibg=#60a0c0 gui=NONE term=NONE cterm=NONE ctermfg=NONE ctermbg=4
  hi CursorLine     guifg=NONE    guibg=#1c1c1c gui=NONE term=NONE cterm=NONE ctermfg=NONE ctermbg=8
  hi CursorColumn   guifg=NONE    guibg=#1c1c1c gui=NONE term=NONE cterm=NONE ctermfg=NONE ctermbg=8
  hi Debug          guifg=#d7afaf guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=181  ctermbg=NONE
  hi Define         guifg=#7E62B3 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=13   ctermbg=NONE
  hi Delimiter      guifg=#d7afaf guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=109  ctermbg=NONE
  hi DiffAdd        guifg=#5f8787 guibg=#3a3a3a gui=NONE term=NONE cterm=NONE ctermfg=66   ctermbg=237
  hi DiffChange     guifg=NONE    guibg=#5f8787 gui=NONE term=NONE cterm=NONE ctermfg=NONE ctermbg=236
  hi DiffDelete     guifg=#5f8787 guibg=#444444 gui=NONE term=NONE cterm=NONE ctermfg=236  ctermbg=238
  hi DiffText       guifg=#ffafaf guibg=#3a3a3a gui=NONE term=NONE cterm=NONE ctermfg=217  ctermbg=237
  hi Directory      guifg=#d7d7d7 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=188  ctermbg=NONE
  hi ErrorMsg       guifg=#7E62B3 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=13   ctermbg=NONE
  hi Error          guifg=#7E62B3 guibg=#1c1c1c gui=NONE term=NONE cterm=NONE ctermfg=13   ctermbg=234
  hi Exception      guifg=#b2b2b2 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=249  ctermbg=NONE
  hi Float          guifg=#c6c6c6 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=251  ctermbg=NONE
  hi FoldColumn     guifg=#875faf guibg=#444444 gui=NONE term=NONE cterm=NONE ctermfg=97   ctermbg=238
  hi Folded         guifg=#444444 guibg=#1c1c1c gui=NONE term=NONE cterm=NONE ctermfg=238  ctermbg=234
  hi Function       guifg=#875f5f guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=95   ctermbg=NONE
  hi Identifier     guifg=#87afd7 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=110  ctermbg=NONE
  hi IncSearch      guifg=#af5f5f guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=131  ctermbg=NONE
  hi Keyword        guifg=#af87d7 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=12   ctermbg=NONE
  hi Label          guifg=#d7d7af guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=187  ctermbg=NONE
  hi LineNr         guifg=#262626 guibg=#121212 gui=NONE term=NONE cterm=NONE ctermfg=235  ctermbg=233
  hi Macro          guifg=#7E62B3 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=13   ctermbg=NONE
  hi ModeMsg        guifg=#7E62B3 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=13   ctermbg=NONE
  hi MoreMsg        guifg=#C0C0C0 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=15   ctermbg=NONE
  hi NonText        guifg=#444444 guibg=#1c1c1c gui=NONE term=NONE cterm=NONE ctermfg=238  ctermbg=234
  hi Number         guifg=#af87ff guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=141  ctermbg=NONE
  hi Operator       guifg=#00afd7 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=38   ctermbg=NONE
  hi PreCondit      guifg=#d7af87 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=180  ctermbg=NONE
  hi PreProc        guifg=#3955c4 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=12   ctermbg=NONE
  hi Question       guifg=#C0C0C0 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=15   ctermbg=NONE
  hi Repeat         guifg=#af5f5f guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=131  ctermbg=NONE
  hi Search         guifg=#b94062 guibg=NONE	gui=NONE term=NONE cterm=NONE ctermfg=9    ctermbg=NONE
  hi SpecialChar    guifg=#d7afaf guibg=NONE	gui=NONE term=NONE cterm=NONE ctermfg=181  ctermbg=NONE
  hi SpecialComment guifg=#87af87 guibg=NONE	gui=NONE term=NONE cterm=NONE ctermfg=108  ctermbg=NONE
  hi Special        guifg=#af87d7 guibg=NONE	gui=NONE term=NONE cterm=NONE ctermfg=140  ctermbg=NONE
  hi SpecialKey     guifg=#afd7af guibg=NONE	gui=NONE term=NONE cterm=NONE ctermfg=151  ctermbg=NONE
  hi Statement      guifg=#8787af guibg=NONE	gui=NONE term=NONE cterm=NONE ctermfg=103  ctermbg=NONE
  hi StatusLine     guifg=#77b6c5 guibg=#1c1c1c gui=NONE term=NONE cterm=NONE ctermfg=6    ctermbg=234
  hi StatusLineNC   guifg=#1c1c1c guibg=#5f5f5f gui=NONE term=NONE cterm=NONE ctermfg=234  ctermbg=59
  hi StorageClass   guifg=#b2b2b2 guibg=NONE	gui=NONE term=NONE cterm=NONE ctermfg=249  ctermbg=NONE
  hi String         guifg=#E0E0E0 guibg=NONE	gui=NONE term=NONE cterm=NONE ctermfg=7    ctermbg=NONE
  hi Structure      guifg=#ffffaf guibg=NONE	gui=NONE term=NONE cterm=NONE ctermfg=229  ctermbg=NONE
  hi Tag            guifg=#8fb676 guibg=NONE	gui=NONE term=NONE cterm=NONE ctermfg=10   ctermbg=NONE
  hi Title          guifg=#E0E0E0 guibg=NONE	gui=NONE term=NONE cterm=NONE ctermfg=7    ctermbg=NONE
  hi Todo           guifg=#875f5f guibg=NONE	gui=NONE term=NONE cterm=NONE ctermfg=95   ctermbg=NONE
  hi Typedef        guifg=#dadada guibg=NONE	gui=NONE term=NONE cterm=NONE ctermfg=253  ctermbg=NONE
  hi Type           guifg=#878787 guibg=NONE	gui=NONE term=NONE cterm=NONE ctermfg=102  ctermbg=NONE
  hi Underlined     guifg=#d7d7d7 guibg=NONE	gui=NONE term=NONE cterm=NONE ctermfg=188  ctermbg=NONE
  hi VertSplit      guifg=#5f8787 guibg=#5f5f5f gui=NONE term=NONE cterm=NONE ctermfg=236  ctermbg=59
  hi VisualNOS      guifg=#5E468C guibg=NONE	gui=NONE term=NONE cterm=NONE ctermfg=5    ctermbg=NONE
  hi WarningMsg     guifg=#C0C0C0 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=15   ctermbg=NONE
  hi WildMenu       guifg=#d7ffd7 guibg=NONE    gui=NONE term=NONE cterm=NONE ctermfg=194  ctermbg=NONE
endif
hi RedundantWhitespace guifg=NONE guibg=#d74b73 gui=NONE term=NONE ctermfg=NONE ctermbg=1
match RedundantWhitespace /\s\+$\| \+\ze\t/
