function fileTreeToNav(t) {
  const generatedClassName = t.onlyGenerated ? "uhNavGenerated" : "";
  return {
    // Note: id must be unique in the tree.
    id: t.kytheUri,
    name: t.display,
    onlyGenerated: t.onlyGenerated,
    isFile: t.isFile,
    children: t.children == null ? null : t.children.map(fileTreeToNav),
    open: false,
    highlight: false,
  };
}

export default {
  fileTreeToNav,
}

