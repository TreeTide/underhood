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
    case "yaml":
      return "text/x-yaml";
    default:
      return lng;
  }
}

export default {
  backendProgLangToCodeMirror,
}

