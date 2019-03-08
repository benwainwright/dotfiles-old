REITH_ON="BBC On Network"
REITH_OFF="BBC Off Network"

detect_and_configure_reith_proxy_if_present() {
  if [ "$REITH_ON" = "$(networksetup -getcurrentlocation)" ]; then
    configure_shell_for_reith
  else 
    configure_shell_for_no_reith
  fi
}

reith() {
  if [ "$1" = "on" ]; then
    echo "Switching location to '$REITH_ON' and disabling wifi"
    networksetup -switchtolocation "$REITH_ON" > /dev/null
    networksetup -setairportpower en0 off
    configure_shell_for_reith
  elif [ "$1" = "off" ]; then
    echo "Switching location to '$REITH_OFF' and enabling wifi"
    networksetup -switchtolocation "$REITH_OFF" > /dev/null
    networksetup -setairportpower en0 on
    configure_shell_for_no_reith
  fi
}

configure_shell_for_no_reith() {
  echo "Removing shell proxy configuration"
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
  sed -i ".bak" "s/^\(  ProxyCommand nc\)\(.*$\)/#\1\2/g" ~/.ssh/config.github.reith.socks
}

configure_shell_for_reith() {
  echo "Configuring shell proxy settings"
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
  sed -i ".bak" "s/^\#\{1,1\}\(  ProxyCommand nc\)\(.*$\)/\1\2/g" ~/.ssh/config.github.reith.socks
  export JAVA_OPTS="$JAVA_OPTS
    -Dhttp.proxyHost=$http_proxy_url -Dhttp.proxyPort=$http_proxy_port
    -Dhttps.proxyHost=$http_proxy_url -Dhttps.proxyPort=$http_proxy_port
    -Dhttp.nonProxyHosts=127.0.0.1|localhost|.local|.sandbox.bbc.co.uk|sandbox.bbc.co.uk|.sandbox.dev.bbc.co.uk|sandbox.dev.bbc.co.uk
    -Dhttps.nonProxyHosts=127.0.0.1|localhost|.local|.sandbox.bbc.co.uk|sandbox.bbc.co.uk|.sandbox.dev.bbc.co.uk|sandbox.dev.bbc.co.uk"
}

detect_and_configure_reith_proxy_if_present
