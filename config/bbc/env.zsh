function is_internal_network {
    [ "BBC On Network" = "$(networksetup -getcurrentlocation)" ]
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

export DEV_CERT_LOCATION=/etc/pki/tls/certs/client.crt
export DEV_KEY_LOCATION=/etc/pki/tls/private/client.key
export DEV_PEM_LOCATION=/etc/pki/tls/cert.pem
export DEV_P12_LOCATION=$HOME/workspace/dev.bbc.co.uk.p12

# Needed for cosmos CLI
export COSMOS_CERT=/etc/pki/tls/cert.pem
export COSMOS_CERT_KEY=/etc/pki/tls/private/client.key

export JENKINS_URL=https://jenkins.cd.test.tools.bbc.co.uk

export CHRONOS=kieran-bamforth,andrewscfc,lalkhum,cefn,saralk
