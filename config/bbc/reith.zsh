configure_and_switch_reith_proxy() {
  if [ "BBC On Network" = "$(networksetup -getcurrentlocation)" ]; then
    set_bbc_network on
  else 
    set_bbc_network off
  fi
}

set_bbc_network() {
  if [ "$1" = "on" ];then
    configure_reith_proxy
  elif [ "$1" = "off" ]; then
    disable_reith_proxy
  fi
}

disable_reith_proxy() {
  echo "Disabling BBC corporate network"
  if [ ! -z "$http_proxy" ] || \
     [ ! -z "$HTTP_PROXY" ] || \
     [ ! -z "$https_proxy" ] || \
     [ ! -z "$HTTPS_PROXY" ] || \
     [ ! -z "$FTP_PROXY" ] || \
     [ ! -z "$ftp_proxy" ] || \
     [ ! -z "$npm_config_proxy" ] || \
     [ ! -z "$npm_config_https_proxy" ] || \
     [ ! -z "$ALL_PROXY" ]; then
    unset http_proxy HTTP_PROXY https_proxy \
      HTTPS_PROXY FTP_PROXY ftp_proxy \
      ALL_PROXY npm_config_proxy \
      npm_config_https_proxy
  fi
  if command -v hub > /dev/null; then
    unalias hub 2> /dev/null
  else
    unalias git 2> /dev/null
  fi
  sed -i ".bak" "s/\(ProxyCommand nc\)\(.*$\)/#\1\2/g" ~/.ssh/config
}

configure_reith_proxy() {
  echo "Configuring BBC corporate network"
  export http_proxy_port=80
  export http_proxy_url="www-cache.reith.bbc.co.uk"
  export http_proxy="$http_proxy_url:$http_proxy_port"
  export https_proxy="$http_proxy"
  export ftp_proxy="ftp-gw.reith.bbc.co.uk:21"
  export socks_proxy="socks-gw.reith.bbc.co.uk:1080"
  export HTTP_PROXY="$http_proxy"
  export HTTPS_PROXY="$https_proxy"
  export FTP_PROXY="$ftp_proxy"
  export ALL_PROXY="$http_proxy"
  export NO_PROXY="localhost,127.0.0.1"
  export npm_config_proxy="http://$HTTP_PROXY"
  export npm_config_https_proxy="http://$HTTPS_PROXY"
  if command -v hub > /dev/null; then
    alias hub="hub -c http.proxy=\"$HTTP_PROXY\""
  else
    alias git="git -c http.proxy=\"$HTTP_PROXY\""
  fi
  sed -i ".bak" "s/\#\{1,1\}\(ProxyCommand nc\)\(.*$\)/\1\2/g" ~/.ssh/config
  export JAVA_OPTS="$JAVA_OPTS
    -Dhttp.proxyHost=$http_proxy_url -Dhttp.proxyPort=$http_proxy_port
    -Dhttps.proxyHost=$http_proxy_url -Dhttps.proxyPort=$http_proxy_port
    -Dhttp.nonProxyHosts=127.0.0.1|localhost|.local|.sandbox.bbc.co.uk|sandbox.bbc.co.uk|.sandbox.dev.bbc.co.uk|sandbox.dev.bbc.co.uk
    -Dhttps.nonProxyHosts=127.0.0.1|localhost|.local|.sandbox.bbc.co.uk|sandbox.bbc.co.uk|.sandbox.dev.bbc.co.uk|sandbox.dev.bbc.co.uk"
}

configure_and_switch_reith_proxy
