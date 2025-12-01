# Note: solarized is special and added manually.
for t in \
  darcula \
  idea \
  monokai \
  night \
  the-matrix \
  zenburn
do

  cat node_modules/codemirror/theme/$t.css | scripts/extract_theme.sh $t > static/css/uh-$t.css
done
