<template>
  <div :class="_appClasses">
    <!-- TODO(robinp): better automated sizing between header and rest -->
    <Header class="header" :current-ticket="renderedTicket" :bus="mkThemeBus" />
    <splitpanes horizontal class="default-theme top-split"
        @resized="onTopSplitResized"
        @resize="onTopSplitResize"
        >
      <pane :key="1" size="75">
        <splitpanes class="default-theme">
          <pane :key="11" size="20" class="filetree-pane">
            <div v-if="nodes" class="uh-background uh-color fullHeight">
              <file-tree
                v-for="top in nodes.children"
                :key="top.id"
                :model="top"
                :bus="mkNavBus"
                />
            </div>
          </pane>
          <pane :key="12" size="80" class="viewer-pane">
            <codemirror class="viewer"
              :value="code"
              :options="cmOptions"
              ref="myCm"
              @ready="onCmReady"
              @scroll="onCmScroll"
              @viewportChange="onCmViewportChange"
              @cursorActivity="onCmCursorActivity"
              />
            <pre class="tooltip" id="doc_tooltip"></pre>
          </pane>
        </splitpanes>
      </pane>
      <pane :key="2" size="25" class="refs-pane">
        <References
            :bus="mkRefBus"
            :ticket="refTicket"
            :ref-data="refData"
            :highlight-mode="cmOptions.mode"
            :highlight-style="cmOptions.theme" />
        <div :class="'uh-background refs-filler'" />
      </pane>
    </splitpanes>
  </div>
</template>

<script>
import axios from 'axios';
import Vue from 'vue';
//
import CodeMirror from 'codemirror';
import 'codemirror/lib/codemirror.css';
// Language syntax modules
import 'codemirror/mode/clike/clike.js';
import 'codemirror/mode/python/python.js';
import 'codemirror/mode/go/go.js';
import 'codemirror/mode/haskell/haskell.js';
// Themes
import 'codemirror/theme/darcula.css';
import 'codemirror/theme/idea.css';
import 'codemirror/theme/monokai.css';
import 'codemirror/theme/night.css';
import 'codemirror/theme/solarized.css';
import 'codemirror/theme/the-matrix.css';
import 'codemirror/theme/zenburn.css';
// Our corresponding themes
import '../static/css/base.css';
import '../static/css/uh-darcula.css';
import '../static/css/uh-idea.css';
import '../static/css/uh-monokai.css';
import '../static/css/uh-night.css';
import '../static/css/uh-solarized-dark.css';
import '../static/css/uh-solarized-light.css';
import '../static/css/uh-the-matrix.css';
import '../static/css/uh-zenburn.css';
// Needed by.. search?
import 'codemirror/addon/dialog/dialog.css';
import 'codemirror/addon/dialog/dialog.js';
//
import 'codemirror/addon/search/search.js';
import 'codemirror/addon/search/searchcursor.js';
import 'codemirror/addon/search/jump-to-line.js';
import 'codemirror/addon/search/matchesonscrollbar.js';
import 'codemirror/addon/search/matchesonscrollbar.css';
import 'codemirror/addon/scroll/annotatescrollbar.js';
//
import { codemirror } from 'vue-codemirror'

import { Splitpanes, Pane } from 'splitpanes'
import 'splitpanes/dist/splitpanes.css'

import RH from './rest_helpers.js'
import FileTree from './FileTree.vue'
import References from './References.vue'
import Header from './Header.vue'

// Warning: this is a shared instance? Singleton.

let xrefState = {
  // CM Line -> [Decor]
  lineToDecors: {},
  // ticket -> [Decor]
  ticketToDecors: {},
  // [CM TextMarker]
  highlightMarkers: [],
  // Timer
  tooltipTimer: null,
  //
  scrollbarUpdater: null,
  //
  lineClasses: [],
}

function resetXRefState(cm) {
  xrefState.lineToDecors = {};
  xrefState.ticketToDecors = {};
  xrefState.highlightMarkers = [];
  xrefState.tooltipTimer = null;
  clearHighlightMarkers(cm);
  clearLineClasses(cm);
}

function addLineClass(cm, l, cls) {
  xrefState.lineClasses.push(cm.getDoc().addLineClass(l, "wrap", cls));
}

function clearLineClasses(cm) {
  const doc = cm.getDoc();
  for (let i in xrefState.lineClasses) {
    doc.removeLineClass(xrefState.lineClasses[i], "wrap");
  }
  xrefState.lineClasses = [];
}

// Adds 'v' to the list of values held at 'k' in map 'mp'. Creates the list
// if not yet present.
function pushToKey(mp, k, v) {
  if (!mp[k]) {
    mp[k] = [v];
  } else {
    mp[k].push(v);
  }
}

function addXRef(decor) {
  if (decor.dStart.line == decor.dEnd.line) {
    pushToKey(xrefState.lineToDecors, decor.dStart.line, decor);
    pushToKey(xrefState.ticketToDecors, decor.dTarget, decor);
  }
}

function lookupXRef(pos) {
  let xs = xrefState.lineToDecors[pos.line];
  if (!xs) return [];
  let res = [];
  for (let i in xs) {
    let x = xs[i];
    if (x.dStart.ch <= pos.ch && x.dEnd.ch > pos.ch) {
      res.push(x);
    }
  }
  return res;
}

function clearHighlightMarkers(cm) {
  timed("clear-highlight", () => cm.operation(function() {
    for (let i in xrefState.highlightMarkers) {
      xrefState.highlightMarkers[i].clear();
    }
    if (xrefState.scrollbarUpdater) {
      xrefState.scrollbarUpdater.clear();
      xrefState.scrollbarUpdater = null;
    }
  }));
  xrefState.highlightMarkers = [];
}

function highlightXRef(cm, ticket) {
  clearHighlightMarkers(cm);
  const ds = xrefState.ticketToDecors[ticket];
  if (!ds) return;
  timed("highlight-xrefs", () => cm.operation(function() {
    // Annotate scrollbar, using annotatescrollbar.js.
    let hls = [];
    // TODO this can take somewhat long (~100ms) on larger docs, so do the
    //   lazy view-based highlight here as well. Also with clearing - clearing
    //   markers takes similar time.
    for (let i in ds) {
      const cmStart = ds[i].dStart;
      const cmEnd = ds[i].dEnd;
      const marker = cm.markText(cmStart, cmEnd, {
        className: 'uh-color-inverted uh-background-inverted',
      });
      xrefState.highlightMarkers.push(marker);
      hls.push({
        from: cmStart,
        to: cmEnd
      });
    }
    xrefState.scrollbarUpdater = cm.annotateScrollbar('CodeMirror-search-match');
    xrefState.scrollbarUpdater.update(hls);
  }));
}

function timed(name, f) {
  const t0 = Date.now();
  const res = f();
  const t1 = Date.now();
  console.log(name, t1 - t0);
  return res;
}

function chooseDecorStrategy(ds) {
  // Implements tightest-span for now.
  let mn = Infinity;
  let res = null;
  for (let i in ds) {
    if (ds[i].dTarget.includes('_test')) {
      // This is a HACK - for Kythe repo specially.
      // Try on 'kythe/go/platform/delimited/dedup/dedup.go'
      //
      // TODO(robin): go code (and maybe others) often puts multiple refs at the same
      // span (when a package spans multiple files, a ref into each path is emitted,
      // short of better options). So we should fetch (&merge?) all results from these.
      //
      // Q: how do we display these? Tabs? Tables?
      continue;
    }
    let span = ds[i].dSpan;
    // HACK note: we should check the edge type, and choose based on that.
    // Or display a multi-edge menu.
    if (span && span <= mn) {  // HACKed to choose last edge (what is that?)
      mn = span;
      res = ds[i];
    }
  }
  if (res) return res;
  // Fallback.
  if (ds.length) return ds[ds.length-1];  // HACK, should choose based on edge
  return null;
}

let looseLog = _.debounce(function(cm) {
  let si = cm.getScrollInfo();
  let l1 = cm.lineAtHeight(0);
  let l2 = cm.lineAtHeight(si.clientHeight);
  let v = cm.getViewport();
  console.log(si.top, si.clientHeight, l1, l2, "vp", v);
 }, 25);

export default {
  props: {
    ticket: String,
    line: Number,  // Nullable
    tooltipDelayMs: {
      type: Number,
      default: 300,
    },
  },
  data () {
    return {
      nodes: null,
      code: '// Please wait for filenav to load on the left and select file.',
      refTicket: null,
      refData: null,
      renderedTicket: null,
      cmOptions: {
        mode: 'text/x-go',
        undoDepth: 0,
        lineNumbers: true,
        theme: 'zenburn',
        // TODO: with Infinitiy, full-page-search works, but rendering and
        //   adding xrefs to big docs gets slow. We should add our own search,
        //   then can ditch Infitity, so we get speedup.
        viewportMargin: 10,  // Infinity,
        readOnly: true,
        cursorBlinkRate: -1,
      },
    }
  },
  methods: {
    onTopSplitResized(ev) {
      const cmPercentage = ev[0].size;
      this.onCodeMirrorHeightPercentage(cmPercentage);
    },
    onTopSplitResize(ev) {
      const cmPercentage = ev[0].size;
      this.onCodeMirrorHeightPercentage(cmPercentage);
    },
    onCodeMirrorHeightPercentage(cmPercentage) {
      // https://stackoverflow.com/questions/1248081/how-to-get-the-browser-viewport-dimensions
      const vh = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);
      const cmHeight = vh * cmPercentage / 100;
      console.log("resize", cmPercentage, "vh", vh, "cmHeight", cmHeight);
      this.codemirror.setSize(null, cmHeight + "px");
    },
    onCmReady (cm) {
      // Default codemirror.css specifies 300px.
      //
      // NOTE: it is important to keep this value fixed (that is, not auto).
      // See https://github.com/TreeTide/underhood/issues/21.
      //
      // TODO(robinp): take initial value from data maybe
      this.onCodeMirrorHeightPercentage(75);

      cm.on('mousedown', this.onCmMouseDown);
      cm.on('touchstart', this.onCmTouchStart);
      cm.on('keydown', this.onCmKeyDown);
      const thiz = this;
      cm.getWrapperElement().addEventListener('mousemove', _.debounce(
        function(e) {
          const cmPos = cm.coordsChar({
            left: e.pageX,
            top: e.pageY
          })
          if (cmPos) {
            thiz._onHoverAt(cmPos, e);
          }
        },
        20));
      cm.getWrapperElement().addEventListener('mousemove', _.debounce(
        function(e) {
          const cmPos = cm.coordsChar({
            left: e.pageX,
            top: e.pageY
          })
          thiz._onHoverEnd(cmPos);
        },
        10,
        {
          leading: true,
          trailing: false,
        }));
    },
    onCmScroll (cm) {
      looseLog(cm);
    },
    onCmViewportChange (cm, e) {
      console.log('viewport-change', e);
    },
    onCmMouseDown (cm, e) {
      console.log('mouse-down', e);
      if (e.ctrlKey) {
        let lineCh = cm.coordsChar({
          left: e.clientX,
          top: e.clientY,
        }, "window");
        let w = cm.findWordAt(lineCh);
        let r = cm.getRange(w.anchor, w.head);
        console.log("ctrl-click on word", "[" + r + "]");
        this._startSearchXref(r, e.shiftKey);
      }
    },
    onCmKeyDown (cm, e) {
      const selText = this.codemirror.getSelection().trim();
      console.log('key-down', e, "[" + selText + "]");
      // TODO configurable key.
      if (selText.length > 0 && e.code == "KeyB") {
        this._startSearchXref(selText, e.shiftKey);
      }
    },
    _startSearchXref(toSearch, invertCaseBehavior) {
      // Trigger a selection-based search.
      // We interpret 'toSearch' casing in a Zoekt-compatible way:
      //
      //  - small caps means ignore case
      //  - mixed/upper caps means keep case
      //
      // If invertCaseBehavior is true, will explicitly request opposite
      // Zoekt case behavior.
      let zoektCase = "auto";
      if (invertCaseBehavior) {
        let lowered = toSearch.toLowerCase();
        let wasIgnoreCase = toSearch == lowered;
        zoektCase = wasIgnoreCase ? "yes" : "no";
      }
      // TODO set spinner on refs?
      axios.get('/api/search-xref', {
        params: {
          selection: toSearch,
          casing: zoektCase,
          ticket: this.renderedTicket,
        },
        // TODO cancelToken / canceller
      })
        .then(response => {
          console.log('updating refData')
          this.refData = {
            refCounts: response.data.refCounts,
            refs: response.data.refs,
            callCount: response.data.callCount,
            calls: response.data.calls,
            definitions: response.data.definitions,
            declarations: response.data.declarations,
          };
        })
        .catch(err => {
          if (!axios.isCancel(err)) {
            console.log(err);
          }
        }).then(() => {
          // remove canceller
          // remove spinner?
        });
    },
    onCmTouchStart (e) {
      console.log('touch-start', e);
    },
    onCmCursorActivity (cm) {
      const c = cm.getDoc().getCursor();
    },
    _onHoverAt(cmPos, mousePos) {
      const decors = lookupXRef(cmPos);
      if (decors.length) {
        if (decors.length > 2) {
          console.log("Mulitple choices", decors)
        }
        // TODO use pluggable logic to choose.
        const ticket = chooseDecorStrategy(decors).dTarget;
        // TODO convert imperative stuff to events, let other components add
        //   these effects and more.
        this._highlightXRefUse(ticket);
        this._lookupDoc(ticket, mousePos);
        this.refTicket = ticket;  // TODO on click
      }
    },
    _onHoverEnd(cmPos) {
      // If we start moving the mouse, we are likely finished reading the doc
      // tooltip.
      if (xrefState.tooltipTimer) {
        // Don't show if was scheduled.
        clearTimeout(xrefState.tooltipTimer);
        xrefState.tooltipTimer = null;
      }
      this._hideDocTooltip();
    },
    _lookupDoc(ticket, mousePos) {
      this._docLookupStart = Date.now();
      axios.get('/api/doc', {
        params: { ticket }
      })
        .then(response => {
          if (response.data.docText) {
            const fetchTook = Date.now() - this._docLookupStart;
            // We defer displaying the tooltip until sufficient hovering.
            const untilDisplay = Math.max(0, this.tooltipDelayMs - fetchTook);
            console.log('millis-until-hover-display', untilDisplay);
            xrefState.tooltipTimer = setTimeout(function() {
              const tt = document.getElementById('doc_tooltip');
              tt.style.left = mousePos.clientX + 'px';
              tt.innerText = response.data.docText;
              tt.style.display = "block";
              // TODO use cm line height instead magic const
              tt.style.top = (mousePos.clientY - tt.offsetHeight - 30) + 'px';
            }, untilDisplay);
          }
        })
        .catch(err => console.log(err));
    },
    _hideDocTooltip() {
      const tt = document.getElementById('doc_tooltip');
      tt.innerText = "";
      tt.style.display = "none";
    },
    _highlightXRefUse(ticket) {
      highlightXRef(this.codemirror, ticket);
    },
    _removeXRefHighlight() {
      clearHighlightMarkers(this.codemirror);
    },
    // Give back focus to main codemirror, so search and keyboard shortcuts
    // can be triggered.
    _giveBackFocus() {
      this.codemirror.focus();
    },
    onNavClick (id) {
      const ticket = id;
      this._loadSource(ticket);
      this._giveBackFocus();
    },
    onLoadMoreTree(id, model, maybeContinuation) {
      // HACK - see [branch version]
      const versionlessId = id.split("@")[0]
      console.log("fetching filetree", id, versionlessId)
      axios.get('/api/filetree?top=' + versionlessId)  // TODO pass as param
        .then(response => {
          this.$set(model, "children", RH.fileTreeToNav(response.data).children);
          if (maybeContinuation) {
            maybeContinuation(model);
          }
        })
        .catch(err => console.log(err));
    },
    onTheme (theme) {
      this.cmOptions.theme = theme;
    },
    onRefClick (filePath) {
      // This filetree model handling should eventually go to a top-level
      // filetree component.
      console.log('should open', filePath);
      let parts = filePath.split("/");
      let i = 0;
      let go = (cur, k) => {
        if (cur.isFile) return;
        if (cur.children == null) {
          // Need to make network trip to API, since this part of tree is not
          // yet loaded. Will call back to this function with the node where
          // children are now present.
          this.onLoadMoreTree(cur.id, cur, (c) => go(c,k));
        } else {
          // TODO eventually store in map if gets slow? Below edge-case makes
          //  that slightly more cumbersome.
          for (let c of cur.children) {
            // Edge-case: paths at top-level of tree can contain slash-separated
            // parts. Zoekt-underhood returns the repos at the top level, and
            // the repo name can contain slashes.
            //
            // Arguably zoekt-underhood could preprocess the repos into a tree,
            // but let's see.
            console.log(c.name, i, parts[i])
            if (c.name == parts[i]) {
              // Normal case
              console.log('found', c.name);
              c.open = true;
              i += 1;
              if (i == parts.length) {
                k(c);
              } else {
                go(c, k);
              }
              return;
            } else if (i == 0 && c.name.startsWith(parts[i])) {
              // Maybe repo edge-case
              // HACK: remove the @version part from the end for now, until
              //   the ref click supplies the specific version too.
              //   See [branch version]
              const versionlessName = c.name.split("@")[0]
              const repoParts = versionlessName.split("/");
              let allMatch = true;
              for (let j = 0; j < repoParts.length; ++j) {
                if (repoParts[j] != parts[i+j]) {
                  allMatch = false;
                  break;
                }
              }
              if (allMatch) {
                console.log('found slashy', c.name)
                c.open = true;
                i += repoParts.length;
                go(c, k);
              }
            }
          }
        }
      }
      // Need nextTick in continuation, since 'go' potentially sets the
      // recursive model, and this is the first chance for Vue to observe its
      // properties.  So if we want FileTree's watcher to detect the change in
      // highlight, we need to defer that update.
      go(this.nodes, (c) => Vue.nextTick(() => {
        console.log('hiliting', c.name);
        c.highlight = true;
        this._giveBackFocus();
      }));
    },
    _loadSource (ticket, mbLineToFocus) {
      if (this.renderedTicket == ticket) {
        console.log('same-ticket');
        if (mbLineToFocus) {
          clearLineClasses(this.codemirror);
          this._jumpToLine(mbLineToFocus);
        }
        return;
      }
      console.log('load-source-start');
      axios.get('/api/source', {
        params: { ticket }
      })
        .then(response => {
          console.log('loaded-source');
          let start = Date.now();
          resetXRefState(this.codemirror);
          this.code = response.data;
          //
          if (mbLineToFocus) {
            console.log('line-to-focus', mbLineToFocus);
            this.$nextTick(() => {
              // Deferred, since codemirror needs to render.
              this._jumpToLine(mbLineToFocus);
            });
          }
          // TODO copy initial prop to data elem to avoid warning about
          //   prop mutation (though it is not well-founded in this case,
          //   since :ticket comes directly from the route which we modify).
          this.ticket = ticket;
          this.renderedTicket = ticket;
          this.$router.push({
            name: 'file',
            params: { ticket, line: mbLineToFocus },
          });
          this.$nextTick(function() {
            console.log('codemirror rendered in', Date.now() - start, Date.now());
          });
          // TODO open and scroll the filetree to the source we navigated to?
          //   Now that happens on explicit Ref click, but might make more sense
          //   to it from here.
          console.log('fetch-decors');
          axios.get('/api/decor', {
                  params: { ticket }
          })
            .then(response => {
              console.log('got-decors');
              this.markupRefs(response.data);
            })
            .catch(err => console.log(err));
        })
        .catch(err => console.log(err));
    },
    _jumpToLine(line) {
        const cmLine = line - 1;
        addLineClass(this.codemirror, cmLine, "uh-activeline-background");
        const margin = this.codemirror.getScrollInfo().clientHeight;
        this.codemirror.scrollIntoView({
          line: cmLine,
          ch: 0,
        }, /* vertical pixels around */ margin/2);
    },
    markupRefs (rs) {
      console.log('start-markup; xrefs to add', rs.decors.length);
      let cm = this.codemirror;
      let ds = _.chunk(rs.decors, 1000);
      // TODO lazily load the xrefs based on screen position. See looseLog.
      function go() {
        let part = ds.shift();
        if (part) {
          let start = Date.now();
          cm.operation(function() {
            for (let i in part) {
              if (part[i].dStart.line != part[i].dEnd.line) {
                // 'defines' scope with full body, or 'ref/call'.
                // Not interesting for now.
                continue;
              }
              const cls = addXRef(part[i]);
              cm.markText(part[i].dStart, part[i].dEnd, {
                className: "baseXRef" + (cls ? (" " + cls) : "")
              });
            }
          })
          let end = Date.now();
          console.log('xref-add-chunk took', end-start);
          setTimeout(go, 20);
        } else {
          console.log('done-morkup');
        }
      };
      go();
    },
    __render: _.debounce(function() {
      // Debounced so different param changes observe only 1 reload.
      this._loadSource(this.ticket, this.line);
    }, 5),
  },
  computed: {
    mkNavBus () {
      return {
        onClick: this.onNavClick,
        onLoadMoreTree: this.onLoadMoreTree,
      }
    },
    // Note: will be gone for vuex eventually.
    mkThemeBus () {
      return {
        onTheme: this.onTheme,
      }
    },
    mkRefBus () {
      return {
        onRefClick: this.onRefClick,
      }
    },
    codemirror() {
      return this.$refs.myCm.codemirror
    },
    _appClasses() {
      // cmThemeClass for the semantic-like highlights, like used in the filetree
      return ['app', this._uhThemeClass, this._cmThemeClass];
    },
    _uhThemeClass() {
      // Note: space replace for "solarized light" and "... dark"
      return 'uh-s-' + this.cmOptions.theme.replace(' ', '-');
    },
    _cmThemeClass() {
      return 'cm-s-' + this.cmOptions.theme.split(' ')[0];
    },
  },
  watch: {
    '$route.params.ticket': function() {
      this.__render();
    },
    '$route.params.line': function() {
      this.__render();
    },
  },
  created () {
    axios.get('/api/filetree')
      .then(response => {
        this.nodes = RH.fileTreeToNav(response.data);
      })
      .catch(err => console.log(err));
    this.__render();
  },
  components: {
    FileTree,
    codemirror,
    References,
    Header,

    Splitpanes,
    Pane,
  },
}
</script>

<style>
.app {
  font-size: 13px;
  font-family: monospace;
  overflow-x: hidden;
}

.splitpanes__pane {
  background: white !important;
}

.header {
  height: 2vh;
}
.top-split {
  width: 100vw;
  height: 98vh;
}

.filetree-pane {
  overflow: auto;
}

.refs-pane {
  overflow: scroll;
  display: flex;
  flex-direction: column;
}

.refs-filler {
  flex: 1 0 auto;
  height: auto;
}

.baseXRef {
  text-decoration: underline;
  cursor: pointer;
}
.xrefHighlight {
  background: #ede6b7;
}
.tooltip {
  position: fixed;
  overflow: hidden;
  display: none;
  padding: 5px;
  background: #dde;
  z-index: 1000;
}
.landing-line {
  background: #ffeed2;
}
.fullHeight {
  height: 100%;
  overflow: auto; /* Would be nicer with unset, but then needs flex filler */
}
</style>
