configure_and_switch_reith_proxy() {
  if [ "BBC On Network" = "$(networksetup -getcurrentlocation)" ]; then
    set_bbc_network on
  else 
    set_bbc_network off
  fi
}

set_bbc_network() {
  if [ "$1" = "on" ];then
    echo "Configuring BBC corporate network"
    configure_reith_proxy
  elif [ "$1" = "off" ]; then
    if [ ! -z "$http_proxy" ] || \
       [ ! -z "$HTTP_PROXY" ] || \
       [ ! -z "$https_proxy" ] || \
       [ ! -z "$HTTPS_PROXY" ] || \
       [ ! -z "$FTP_PROXY" ] || \
       [ ! -z "$ftp_proxy" ] || \
       [ ! -z "$ALL_PROXY" ]; then
         echo "Disabling BBC corporate network"
         unset http_proxy HTTP_PROXY https_proxy \
           HTTPS_PROXY FTP_PROXY ftp_proxy \
           ALL_PROXY
    fi
    unalias npm 2> /dev/null
  fi
}

configure_reith_proxy() {
    export http_proxy="www-cache.reith.bbc.co.uk:80"
    export https_proxy="www-cache.reith.bbc.co.uk:80"
    export ftp_proxy="ftp-gw.reith.bbc.co.uk:21"
    export HTTP_PROXY="$http_proxy"
    export HTTPS_PROXY="$https_proxy"
    export FTP_PROXY="$ftp_proxy"
    export ALL_PROXY="$http_proxy"
    export NO_PROXY="localhost,127.0.0.1"
    alias npm="npm --proxy \"$HTTP_PROXY\" --https-proxy \"$HTTPS_PROXY\""
    alias git="git -c http.proxy=\"$HTTP_PROXY\""
}

configure_and_switch_reith_proxy
