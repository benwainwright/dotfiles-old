" For all .feature files, automatically change the
" vim-test test#project_root variable to equal the
" directory of the Gemfile. This makes sure that
" bundle exec cucumber works correctly
let s:filepath = findfile('Gemfile', '**')
let s:parts = split(s:filepath, '/')
let s:dirparts = s:parts[0:-2]
let s:dir = join(s:dirparts, '/')
let test#project_root = s:dir

