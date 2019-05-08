
export DEV_CERT_LOCATION=/etc/pki/tls/certs/client.crt
export DEV_KEY_LOCATION=/etc/pki/tls/private/client.key
export DEV_PEM_LOCATION=/etc/pki/tls/cert.pem
export DEV_P12_LOCATION=$HOME/workspace/dev.bbc.co.uk.p12

# Needed for cosmos CLI
export COSMOS_CERT=/etc/pki/tls/cert.pem
export COSMOS_CERT_KEY=/etc/pki/tls/private/client.key

export JENKINS_URL=https://jenkins.cd.tools.bbc.co.uk

export CHRONOS=kieran-bamforth,andrewscfc,lalkhum,cefn,saralk
export BBC_WORKSPACE=$HOME/workspace
export MAGIC_REITH_SSH_CONFIG="$HOME/.ssh/config.reith.socks"

# This allows PIP to use the BBC cloud cert as well as public CAs
# Do this with the following set of commands
# curl --location https://curl.haxx.se/ca/cacert.pem >/tmp/cert.pem
# curl --location https://curl.haxx.se/ca/cacert.pem >/tmp/cert.pem
# mv /tmp/cert.pem /usr/local/etc/openssl/cert.pem
export PIP_CERT=/usr/local/etc/openssl/cert.pem
