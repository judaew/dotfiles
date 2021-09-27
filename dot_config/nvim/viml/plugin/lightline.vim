function! LightlineReadonly()
  return &readonly && &filetype !~# '\v(help|man)' ? 'RO' : ''
endfunction

let g:lightline = {
      \ 'colorscheme': 'molokai',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename', 'modified' ],
      \             [ 'gitbranch' ] ],
      \   'right': [ [ 'lineinfo' ],
      \              [ 'filetype' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'fugitive#head',
      \   'readonly': 'LightlineReadonly',
      \ },
      \ 'component': {
      \   'lineinfo': '%l:%c %p%%'
      \ },
      \ }
