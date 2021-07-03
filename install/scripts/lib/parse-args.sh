parse_args() {
  while [[ $# -gt 0 ]]; do
    case "$1" in

      --iterm)
        iterm=true
        shift
        ;;
      
      --python)
        python=true
        shift
        ;;

      --all)
        all=true
        shift
        ;;

      --brew)
        brew=true
        shift
        ;;

      --defaults)
        defaults=true
        shift
        ;;

      --fonts)
        fonts=true
        shift
        ;;

      --symlink)
        symlink=true
        shift
        ;;

      *)
        shift
        ;;
    esac
  done
}

