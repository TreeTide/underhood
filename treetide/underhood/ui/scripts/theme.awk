BEGIN {
  /* NOTE: solarized.css mixes light and dark, so is an outlier. Extract that manually rather than using this simple script. It is not worth the code complication.  */

  theme_style = "cm-s-"theme;
  bg_regexp = "background: ([^;]+);";
  bg_col_regexp = "background-color: ([^;]+);";
  /* Note: exclusion to avoid matching background-color. */
  color_regexp = "[^-]color: ([^;]+);";
}

function to_css(re, name, idx, pre, post) {
  res = match($0, re, parts);
  if (res) {
    print ".uh-s-"theme" .uh-"name" { "pre""parts[idx || 0]""post" }";
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

  if (!to_css(bg_regexp, "color-inverted", 1, "color: ", " !important ;")) {
    to_css(bg_col_regexp, "color-inverted", 1, "color: ", " !important ;");
  }
  to_css(color_regexp, "background-inverted", 1, "background: ", " !important ;");
}

$0 ~ ".CodeMirror-activeline-background" {
  to_css(bg_regexp, "activeline-background");
}

$0 ~ "span::selection" {
  to_css(bg_regexp, "selection-background");
}

$0 ~ theme_style" *div.CodeMirror-selected" {
  to_css(bg_regexp, "selected-background");
  to_css(color_regexp, "selected-color");  /* Note: rarely present */
}

$0 ~ ".CodeMirr.r-linenumber" {
  /* wildcard is for typo in darcula */
  to_css(color_regexp, "linenumber");
}
