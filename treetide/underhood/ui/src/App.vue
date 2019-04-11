<template>
  <div class="app">
    <div class="vertical">
      <div class="top_panel">
        CodeUnderHood code browser by robinp@treetide.com.
        Sign up for updates at <a href="https://treetide.com">treetide.com</a>.
      </div>
      <div class="horizontal">
        <div class="filetree">
          <template v-if="nodes">
            <file-tree
              v-for="top in nodes.children"
              :key="top.id"
              :model="top"
              :bus="mkNavBus"
              />
          </template>
        </div>
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
      </div>
      <div class="bottom_panel">
        <References
            :ticket="refTicket"
            highlight-mode="go"
            highlight-style="solarized" />
      </div>
    </div>
  </div>
</template>

<script>
import axios from 'axios';
//
import CodeMirror from 'codemirror';
import 'codemirror/lib/codemirror.css';
import 'codemirror/mode/clike/clike.js';
import 'codemirror/mode/go/go.js';
import 'codemirror/theme/monokai.css';
import 'codemirror/theme/solarized.css';
import 'codemirror/theme/idea.css';
//
//import 'codemirror/addon/search/matchesonscrollbar.js';
import 'codemirror/addon/search/matchesonscrollbar.css';
import 'codemirror/addon/scroll/annotatescrollbar.js';
//
import { codemirror } from 'vue-codemirror'

import RH from './rest_helpers.js'
import FileTree from './FileTree.vue'
import References from './References.vue'

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
        className: 'xrefHighlight',
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
    let span = ds[i].dSpan;
    if (span && span < mn) {
      mn = span;
      res = ds[i];
    }
  }
  if (res) return res;
  // Fallback.
  if (ds.length) return ds[0];
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
      code: '// Foobar\n// Baz\nint main() {}',
      refTicket: null,
      cmOptions: {
        mode: 'go',
        lineNumbers: true,
        theme: 'solarized',
        // TODO: with Infinitiy, full-page-search works, but rendering and
        //   adding xrefs to big docs gets slow. We should add our own search,
        //   then can ditch Infitity, so we get speedup.
        viewportMargin: Infinity,
        readOnly: true,
        cursorBlinkRate: -1,
      },
    }
  },
  methods: {
    onCmReady (cm) {
      // Default codemirror.css specifies 300px.
      cm.setSize(null, '100%');
      cm.on('mousedown', this.onCmMouseDown);
      cm.on('touchstart', this.onCmTouchStart);
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
    onCmViewportChange (e) {
      console.log('vpc', e);
    },
    onCmMouseDown (e) {
      console.log('md', e);
    },
    onCmTouchStart (e) {
      console.log('tc', e);
    },
    onCmCursorActivity (cm) {
      const c = cm.getDoc().getCursor();
    },
    _onHoverAt(cmPos, mousePos) {
      const decors = lookupXRef(cmPos);
      if (decors.length) {
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
            console.log('ud', untilDisplay);
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
    onNavClick (id) {
      const ticket = RH.idUnescape(id);
      this._loadSource(ticket);
    },
    _loadSource (ticket, mbLineToFocus) {
      axios.get('/api/source', {
        params: { ticket }
      })
        .then(response => {
          let start = Date.now();
          resetXRefState(this.codemirror);
          this.code = response.data;
          //
          if (mbLineToFocus) {
            this.$nextTick(() => {
              const cmLine = mbLineToFocus-1;
              // Deferred, since codemirror needs to render.
              addLineClass(this.codemirror, cmLine, "landing-line");
              const margin = this.codemirror.getScrollInfo().clientHeight;
              this.codemirror.scrollIntoView({
                line: cmLine,
                ch: 0,
              }, /* vertical pixels around */ margin/2);
            });
          }
          // TODO copy initial prop to data elem to avoid warning about
          //   prop mutation (though it is not well-founded in this case,
          //   since :ticket comes directly from the route which we modify).
          this.ticket = ticket;
          this.$router.push({
            name: 'file',
            params: { ticket, line: mbLineToFocus },
          })
          this.$nextTick(function() {
            console.log('Duration', Date.now() - start);
          });
          // TODO open and scroll the filetree to the source we navigated to,
          //   maybe gated by a setting
          axios.get('/api/decor', {
                  params: { ticket }
          })
            .then(response => {
              this.markupRefs(response.data);
            })
            .catch(err => console.log(err));
        })
        .catch(err => console.log(err));
    },
    markupRefs (rs) {
      let cm = this.codemirror;
      console.log('xrefs to add', rs.decors.length);
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
          console.log('xref add chunk took', end-start);
          setInterval(go, 20);
        }
      };
      go();
    },
    __render() {
      this._loadSource(this.ticket, this.line);
    },
  },
  computed: {
    mkNavBus () {
      return {
        onClick: this.onNavClick,
      }
    },
    codemirror() {
      return this.$refs.myCm.codemirror
    }
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
    References
  },
}
</script>

<style>
.app {
  font-size: 13px;
  font-family: monospace;
}

.vertical {
  width: 100vw;
  height: 100vh;
  display: flex;
  flex-direction: column;

}

.top_panel {
  background: #bcedb1;
}
.horizontal {
  height: 100%;
  display: flex;
  overflow-x: hidden;
  flex: 3;
}
.bottom_panel {
  border-top: 1px solid lightgrey;
  overflow-y: auto;
  flex: 1 1 auto;
  height: 0;
}

.filetree {
  border-right: 1px solid lightgrey;
  height: 100%;
  overflow: scroll;
  flex: 0 0 250px;
}
.viewer {
  overflow-x: hidden;
  flex: 1;
}

.baseXRef {
  text-decoration: underline;
  cursor: pointer;
}
.xrefHighlight {
  background: yellow;
}
.tooltip {
  position: fixed;
  overflow: hidden;
  display: none;
  padding: 5px;
  background: yellow;
  z-index: 1000;
}
.landing-line {
  background: #ffeed2;
}
</style>
