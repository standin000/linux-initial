if has("multi_byte")
" Plato Wu,2015/01/19: vi 7.3 of arm raspberry don't support &termencoding
  if &termencoding == ""
    let &termencoding = &encoding
  endif
  set encoding=utf-8
  setglobal fileencoding=utf-8
  "setglobal bomb
  set fileencodings=ucs-bom,utf-8,latin1
endif
