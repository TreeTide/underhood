BEGIN {
  /* NOTE: solarized.css mixes light and dark, so is an outlier. Extract that manually rather than using this simple script. It is not worth the code complication.  */

  theme_style = "cm-s-"theme;
  bg_regexp = "background: ([^;]+);";
  bg_col_regexp = "background-color: ([^;]+);";
  color_regexp = "color: ([^;]+);";
}

function to_css(re, name) {
  res = match($0, re, parts);
  if (res) {
    print ".uh-s-"theme" .uh-"name" { "parts[0]" }";
  }
  return res;
}

$0 ~ "(\\."theme_style".CodeMirror {|\\."theme_style" {)" {
  /* Sometimes there are deceptful things, like ...CodeMirror ::selection, thus the extra brace.  */
  /* .CodeMirror-less option for zenburn (and maybe others) */

  if (!to_css(bg_regexp, "background")) {
    to_css(bg_col_regexp, "background");
  }
  to_css(color_regexp, "color");
}

$0 ~ ".CodeMirror-activeline-background" {
  to_css(bg_regexp, "activeline-background");
}

$0 ~ "span::selection" {
  to_css(bg_regexp, "selection-background");
}

$0 ~ "div.CodeMirror-selected" {
  to_css(bg_regexp, "selected-background");
  to_css(color_regexp, "selected-color");  /* Note: rarely present */
}
