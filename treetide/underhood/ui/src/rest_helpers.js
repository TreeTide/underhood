// Escaping is needed, one reason is it might contain single-quote, which would
// mess up when embedded in w2's inline event handlers.
function idEscape(s) {
  return btoa(s);
}

function idUnescape(s) {
  return atob(s);
}

function fileTreeToNav(t) {
  const generatedClassName = t.onlyGenerated ? "uhNavGenerated" : "";
  return {
    // Note: id must be unique in the tree.
    id: idEscape(t.kytheUri),
    name: t.display,
    onlyGenerated: t.onlyGenerated,
    children: t.children.map(fileTreeToNav),
    open: false,
    highlight: false,
  };
}

export default {
  idEscape,
  idUnescape,
  fileTreeToNav,
}

