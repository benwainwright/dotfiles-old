# Credit to https://github.com/ijcd/dotfiles/blob/antigen/antigen/antigen_demo.zsh
function dots-start-capture () {
    dots__capture__file=$1
    echo "Starting -antigen-load capture into $dots__capture__file"

    # remove prior cache file
    [ -f "$dots__capture__file" ] && rm -f $dots__capture__file
    
    # save current -antigen-load and shim in a version
    # that logs calls to the catpure file
    eval "function -dots-original$(functions -- -antigen-load)"
    function -antigen-load () {
        echo -antigen-load "$@" >>! $dots__capture__file
        -dots-original-antigen-load "$@"
    }
}

function dots-stop-capture () {
    echo "Captured -antigen-load calls into $dots__capture__file"
    
    # unset catpure file var and restore intercepted -antigen-load
    unset dots__capture__file
    eval "function $(functions -- -dots-original-antigen-load | sed 's/-dots-original//')"
}

ZDOTCACHE=~/.zdotcache
