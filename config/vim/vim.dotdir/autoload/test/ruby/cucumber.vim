


function! test#ruby#cucumber#test_file(file) abort
  if exists(g:test#ruby#cucumber#ruby_glob)
    let l:feature_glob=g:test#ruby#cucumber#ruby_glob
  else
    let l:feature_glob='features/**/*.rb'
  endif

  if a:file =~# g:test#ruby#cucumber#file_pattern
    return !empty(glob('features/**/*.rb'))
  endif
endfunction
