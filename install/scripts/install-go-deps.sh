install_go_deps() {
  if command -v gofumpt > /dev/null; then
    go install mvdan.cc/gofumpt@latest
  fi
}
