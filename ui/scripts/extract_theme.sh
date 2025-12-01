THIS_DIR=$(dirname $(readlink -f $0))
tr '\n' ' ' | sed -e 's/}/}\n/g' -e "s/^ *//" | awk -v theme="$1" -f $THIS_DIR/theme.awk
