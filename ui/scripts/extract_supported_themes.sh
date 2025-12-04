# Note: solarized is special and added manually.
#
# Removed because looks odd (contrast bad?): 3024-day base16-light moxer paraiso-light yeti
echo > imports.txt
echo > options.txt


for t in \
  3024-night \
  abbott \
  abcdef \
  ambiance-mobile \
  ambiance \
  ayu-dark \
  ayu-mirage \
  base16-dark \
  bespin \
  blackboard \
  cobalt \
  colorforth \
  darcula \
  dracula \
  duotone-dark \
  duotone-light \
  eclipse \
  elegant \
  erlang-dark \
  gruvbox-dark \
  hopscotch \
  icecoder \
  idea \
  isotope \
  juejin \
  lesser-dark \
  liquibyte \
  lucario \
  material-darker \
  material-ocean \
  material-palenight \
  material \
  mbo \
  mdn-like \
  midnight \
  monokai \
  neat \
  neo \
  night \
  nord \
  oceanic-next \
  panda-syntax \
  paraiso-dark \
  pastel-on-dark \
  railscasts \
  rubyblue \
  seti \
  shadowfox \
  ssms \
  the-matrix \
  tomorrow-night-bright \
  tomorrow-night-eighties \
  ttcn \
  twilight \
  vibrant-ink \
  xq-dark \
  xq-light \
  yonce \
  zenburn
do

  if [ -f scripts/uh-$t.css ]
  then
    cat scripts/uh-$t.css > static/css/uh-$t.css
  else
    cat node_modules/codemirror/theme/$t.css | scripts/extract_theme.sh $t > static/css/uh-$t.css
    if [ -f scripts/uh-$t-extra.css ]
    then
      # For fixing certain themes where css would be harder to parse.
      cat scripts/uh-$t-extra.css >> static/css/uh-$t.css
    fi
  fi
  echo "import 'codemirror/theme/$t.css';" >> imports.txt;
  echo "import '../static/css/uh-$t.css';" >> imports.txt;
  echo "        <option>$t</option>" >> options.txt;
done

for t in juejin xq-light neat ttcn idea eclipse ambiance-mobile elegant neo
do
  echo ".uh-s-$t .uh-selected-background { background: /* cm-default */ #d7d4f0; }" >> static/css/uh-$t.css
done

