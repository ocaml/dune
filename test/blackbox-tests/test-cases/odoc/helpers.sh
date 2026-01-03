stub_odoc_200() {
  cat > odoc << 'SCRIPT'
#!/bin/bash
case "$1" in
  --version)
    echo "2.0.0"
    ;;
  compile | compile-index)
    output=""
    while [[ $# -gt 0 ]]; do
      if [[ "$1" == "-o" && -n "$2" ]]; then
        output="$2"
        shift 2
      else
        shift
      fi
    done
    if [[ -n "$output" ]]; then
      mkdir -p $(dirname "$output")
      touch "$output"
    fi
    exit 0;;
  link)
    output=""
    while [[ $# -gt 0 ]]; do
      if [[ "$1" == "-o" && -n "$2" ]]; then
        output="$2"
        shift 2
      else
        shift
      fi
    done
    if [[ -n "$output" ]]; then
      mkdir -p $(dirname "$output")
      touch "$output"
    fi
    exit 0;;
  html-generate)
    while [[ $# -gt 0 ]]; do
      if [[ "$1" == "-o" && -n "$2" ]]; then
        mkdir -p "$2"
        break
      fi
      shift
    done
    exit 0;;
  *) exit 0;;
esac
SCRIPT
  chmod +x odoc
  export PATH=".:$PATH"
}

stub_odoc_310() {
  cat > odoc << 'SCRIPT'
#!/bin/bash
case "$1" in
  --version)
    echo "3.1.0"
    ;;
  compile | compile-index)
    output=""
    while [[ $# -gt 0 ]]; do
      if [[ "$1" == "-o" && -n "$2" ]]; then
        output="$2"
        shift 2
      else
        shift
      fi
    done
    if [[ -n "$output" ]]; then
      mkdir -p $(dirname "$output")
      touch "$output"
    fi
    exit 0;;
  link)
    output=""
    while [[ $# -gt 0 ]]; do
      if [[ "$1" == "-o" && -n "$2" ]]; then
        output="$2"
        shift 2
      else
        shift
      fi
    done
    if [[ -n "$output" ]]; then
      mkdir -p $(dirname "$output")
      touch "$output"
    fi
    exit 0;;
  html-generate)
    while [[ $# -gt 0 ]]; do
      if [[ "$1" == "-o" && -n "$2" ]]; then
        mkdir -p "$2"
        break
      fi
      shift
    done
    exit 0;;
  markdown-generate)
    while [[ $# -gt 0 ]]; do
      if [[ "$1" == "-o" && -n "$2" ]]; then
        mkdir -p "$2"
        touch "$2/index.md"
        mkdir -p "$2/foo"
        touch "$2/foo/index.md"
        break
      fi
      shift
    done
    exit 0;;
  *) exit 0;;
esac
SCRIPT
  chmod +x odoc
  export PATH=".:$PATH"
}

stub_odoc_bad_version() {
  cat > odoc << 'SCRIPT'
#!/bin/bash
case "$1" in
  --version)
    echo "not a valid version"
    ;;
  compile | compile-index)
    output=""
    while [[ $# -gt 0 ]]; do
      if [[ "$1" == "-o" && -n "$2" ]]; then
        output="$2"
        shift 2
      else
        shift
      fi
    done
    if [[ -n "$output" ]]; then
      mkdir -p $(dirname "$output")
      touch "$output"
    fi
    exit 0;;
  link)
    output=""
    while [[ $# -gt 0 ]]; do
      if [[ "$1" == "-o" && -n "$2" ]]; then
        output="$2"
        shift 2
      else
        shift
      fi
    done
    if [[ -n "$output" ]]; then
      mkdir -p $(dirname "$output")
      touch "$output"
    fi
    exit 0;;
  html-generate)
    while [[ $# -gt 0 ]]; do
      if [[ "$1" == "-o" && -n "$2" ]]; then
        mkdir -p "$2"
        break
      fi
      shift
    done
    exit 0;;
  *) exit 0;;
esac
SCRIPT
  chmod +x odoc
  export PATH=".:$PATH"
}
