function is_internal_network {
    [ "BBC On Network" = "$(/usr/sbin/scselect 2>&1 | grep '^ \*' | sed 's/.*(\(.*\))/\1/')" ]
}
 
# Export the BBC proxy for curl if we're on BBC internal network
if is_internal_network; then
    echo "BBC corporate network detected; configuring proxy..."
    export http_proxy="http://www-cache.reith.bbc.co.uk:80"
    export https_proxy="http://www-cache.reith.bbc.co.uk:80"
    export HTTPS_PROXY="http://www-cache.reith.bbc.co.uk:80"
    export FTP_PROXY="ftp-gw.reith.bbc.co.uk:21"
    export ftp_proxy="ftp-gw.reith.bbc.co.uk:21"
    export ALL_PROXY="$http_proxy"
fi

