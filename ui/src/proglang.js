function backendProgLangToCodeMirror(pl) {
  const lng = pl.toLowerCase();
  switch (lng) {
    // Non-directly mappable cases here
    case "c":
      return "text/x-csrc";
    case "c++":
      return "text/x-c++src";
    case "html":
      return "text/html";
    case "javascript":
      return "text/javascript";
    case "json":
      return "application/json";
    case "markdown":
      return "text/x-markdown";
    case "protocol buffer":
      return "text/x-protobuf";
    case "python":
      return "text/x-python";
    case "typescript":
      return "application/typescript";
    case "vue":
      // TODO(language-support,vue): for snippet highlight to work, would need
      // to pseudo-wrap (or otherwise influence codemirror parser state) with
      // the style/template/script subsection. And that would need to be
      // retrieved or inferred too.
      return "text/x-vue";
    case "yaml":
      return "text/x-yaml";
    default:
      return lng;
  }
}

export default {
  backendProgLangToCodeMirror,
}

