# Ben's amazing Reith management script
#
# This script does the following:

REITH_ON="BBC On Network"
REITH_OFF="BBC Off Network"

# Query MacOSX `networksetup` and:
# - Run configure_shell_for_reith when on Reith and
# - Run configure_shell_for_no-reith when off Reith
detect_and_configure_reith_proxy_if_present() {
  if [ "$REITH_ON" = "$(networksetup -getcurrentlocation)" ]; then
    configure_shell_for_reith
  else
    configure_shell_for_no_reith
  fi
}

# reith on  - Enable "BBC On Network" location, turn off
#             wifi and configure my shell for reith
# reith off - Enable "BBC Off Network"  location, turn on
#             wifi and configure my shell for not being on
#             reith
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
  else
    echo "usage: reith <on/off>"
  fi
}

# unset all proxy configuration environment variables
# unalias hub if present or git if not
configure_shell_for_no_reith() {
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
  sed -i ".bak" "s/^\([ \t]*ProxyCommand\)\(.*$\)/#\1\2/g" \
      ~/.ssh/config.reith.socks
}

# configure_shell_for_reith - Export proxy configuration environment
#                             variables to work with the BBC Reith network
#                           - If 'hub' is present alias it to pass in proxy
#                           - config via the command line.
#                           - If it isn't, do the same for git instead.
#                           - If ~/.ssh/config.reith.socks is present, edit
#                           - it in place, uncommenting any commented out
#                           - ProxyCommand instructions
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

  if [ -f ~/.ssh/config.reith.socks ]; then
     sed -i ".bak" 's/^\([ \t]*\#[ \t]*\)*\(ProxyCommand\)\(.*$\)/  \2\3/g' \
         ~/.ssh/config.reith.socks
  fi

  export JAVA_OPTS="$JAVA_OPTS
    -Dhttp.proxyHost=$http_proxy_url -Dhttp.proxyPort=$http_proxy_port
    -Dhttps.proxyHost=$http_proxy_url -Dhttps.proxyPort=$http_proxy_port
    -Dhttp.nonProxyHosts=127.0.0.1|localhost|.local|.sandbox.bbc.co.uk|sandbox.bbc.co.uk|.sandbox.dev.bbc.co.uk|sandbox.dev.bbc.co.uk
    -Dhttps.nonProxyHosts=127.0.0.1|localhost|.local|.sandbox.bbc.co.uk|sandbox.bbc.co.uk|.sandbox.dev.bbc.co.uk|sandbox.dev.bbc.co.uk"
}

detect_and_configure_reith_proxy_if_present
