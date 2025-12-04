<template>
  <div :class="_appClasses">
    <!-- TODO(robinp): better automated sizing between header and rest -->
    <Header id="header" :current-ticket="renderedTicket" :bus="mkThemeBus"
        @search-bar-text="onSearchBarText"/>
    <splitpanes horizontal class="default-theme top-split"
        @resized="onTopSplitResized"
        @resize="onTopSplitResize"
        >
      <pane :key="1" :size="vPaneSize">
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
          <pane :key="12" size="80" class="viewer-pane" ref="viewerPane">
            <div ref="cmPre">
              <div id="ticketDisplay" class="uh-background uh-color">{{renderedTicket}}</div>
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
          </pane>
        </splitpanes>
      </pane>
      <pane :key="2" :size="100 - vPaneSize" class="refs-pane">
        <References
            :bus="mkRefBus"
            :ticket="refTicket"
            :ext-loading="refsLoading"
            :ref-data="refData"
            :scroll-on-click="collapseRefsOnNextRefClick"
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
import isEqual from 'lodash/isEqual';
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
import { scrollToLastHilit } from './FileTree.vue'
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
      refsLoading: false,
      collapseRefsOnNextRefClick: false,
      renderedTicket: null,
      searchBarText: "",
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
      lastMirrorMouseEvent: null,
      vPaneSize: 75,
      // Saves vPaneSize when using top-bar search, to restore later.
      previousVPaneSize: null,
      preventFileTreeScroll: false,
    }
  },
  mounted() {
    // Note: using this observer to adjust CodeMirror size, instead of
    // splitpane's native resize event, since latter only triggers when moving
    // the splitter manually, but not during initial render stabilization.
    new ResizeObserver(() => {
      // console.log("observer detected viewerPane resize");
      this.setupCodeMirrorHeight();
    }).observe(this.$refs.viewerPane.$el);
  },
  methods: {
    magicDebug(mx, f) {
      let maxtick = [mx];
      let g = () => {
        f();
        if (--maxtick[0] > 0) {
          Vue.nextTick(() => {
            g();
          });
        }
      }
      g();
      setTimeout(() => {
        console.log('AFTER N MILLIS');
        f();
      }, 200);
    },
    // Leaving around in case. Can be used to determine where in ticks or time
    // some property changes.
    hDebug(s) {
      this.magicDebug(10, () =>
            console.log("Tick " + s, this.$refs.viewerPane.$el.clientHeight))
    },
    debugUI(s) {
      let calcPaneH = rh * this.vPaneSize / 100.0;
      let sci = this.codemirror.getScrollInfo();
      console.log(s,
        "viewerPaneH", this.$refs.viewerPane.$el.clientHeight,
        "vPaneSize", this.vPaneSize,
        "calcPaneH", calcPaneH,
        "cmVisibleH", sci.clientHeight);
    },
    onTopSplitResized(ev) {
      this._giveBackFocus();
    },
    onTopSplitResize(ev) {
      // See usage of ResizeObserver in mounted.
      this.vPaneSize = ev[0].size;
    },
    setupCodeMirrorHeight() {
      const viewerH = this.$refs.viewerPane.$el.clientHeight;
      const preH = this.$refs.cmPre.clientHeight;
      const cmHeight = 1 + Math.floor(viewerH - preH);
      this.codemirror.setSize(null, cmHeight + "px");
    },
    onCmReady (cm) {
      cm.on('mousedown', this.onCmMouseDown);
      cm.on('touchstart', this.onCmTouchStart);
      cm.on('keydown', this.onCmKeyDown);
      const thiz = this;
      cm.getWrapperElement().addEventListener('mousemove', function(e) {
        thiz.lastMirrorMouseEvent = e;
      });
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
      // looseLog(cm);
    },
    onCmViewportChange (cm, e) {
      // console.log('viewport-change', e);
    },
    onSearchBarText(q) {
      this.searchBarText = q;
      this.setWindowTitle(q);
      this._startSearchXrefInMode("QueryFromSearchBar", "Raw", false);
      if (this.previousVPaneSize == null) {
        this.previousVPaneSize = this.vPaneSize;
      }
      // Could set to 0.01, but then the #ticketDisplay can disappear
      // mysteriously when searching. CSS wart?
      this.vPaneSize = 15;
    },
    onCmMouseDown (cm, e) {
      // console.log('mouse-down', e);
      this.lastMirrorMouseEvent = e;
      if (e.ctrlKey) {
        this._startSearchXrefInMode("QueryFromCursor", "Lax", e.shiftKey);
      }
    },
    onCmKeyDown (cm, e) {
      // console.log('key-down', e);
      // TODO configurable keys.
      let invertCase = e.shiftKey;
      let querySource = "";
      let mode = "";
      switch (e.code) {
        case "KeyB":
          querySource = "QueryFromSelection";
          mode = "Boundary";
          break;
        case "KeyV":
          querySource = "QueryFromSelection";
          mode = "Lax";
          break;
        case "KeyX":
          querySource = "QueryFromCursor";
          mode = "Lax";
          break;
        case "KeyC":
          querySource = "QueryFromCursor";
          mode = "Boundary";
          break;
        default:
          return;
      }
      this._startSearchXrefInMode(querySource, mode, invertCase);
    },
    setWindowTitle(part) {
      document.title = part + ' - Underhood';
    },
    _startSearchXrefInMode(querySource, mode, invertCaseBehavior) {
      console.log('Xref search', querySource, mode, invertCaseBehavior);
      let q = ""
      switch (querySource) {
      case 'QueryFromSelection':
        q = this.codemirror.getSelection().trim();
        console.log("query from selection", "[" + q + "]");
        break;
      case 'QueryFromCursor':
        if (this.lastMirrorMouseEvent != null) {
          const cm = this.codemirror;
          let lineCh = cm.coordsChar({
            left: this.lastMirrorMouseEvent.clientX,
            top: this.lastMirrorMouseEvent.clientY,
          }, "window");
          let w = cm.findWordAt(lineCh);
          q = cm.getRange(w.anchor, w.head);
          console.log("query from cursor", "[" + q + "]");
        }
        break;
      case 'QueryFromSearchBar':
        q = this.searchBarText;
        break;
      default:
        throw ('Unknown querySource: ' + querySource)
      }

      if (mode != "Lax" && mode != "Boundary" && mode != "Raw") {
        throw ('Unknown mode: ' + mode)
      }

      if (q.length > 0) {
        this.collapseRefsOnNextRefClick = querySource == 'QueryFromSearchBar';
        this._startSearchXref(q, mode, invertCaseBehavior)
      }
    },
    _startSearchXref(toSearch, mode, invertCaseBehavior) {
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
      this.refData = null;
      this.refsLoading = true;  // TODO counterize

      axios.get('/api/search-xref', {
        params: {
          selection: toSearch,
          casing: zoektCase,
          mode: mode,
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
          this.refsLoading = false;
          // TODO remove canceller
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
      this.preventFileTreeScroll = true;
      this.navigateToFileLineIfNeeded({ ticket: id, line: null });
      this._giveBackFocus();
    },
    onLoadMoreTree(id, model, maybeContinuation) {
      console.log("fetching filetree", id);
      axios.get('/api/filetree?top=' + id)
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
    onRefClick (routeParams) {
      this.navigateToFileLineIfNeeded(routeParams);
      // Start restoring vpane, if needed.
      if (this.previousVPaneSize != null) {
        console.log('restoring vpane');
        // Restore
        this.vPaneSize = this.previousVPaneSize;
        this.previousVPaneSize = null;
        this.setupCodeMirrorHeight();
      }
      // Clear, so after first click no jumping around happens in refs.
      this.collapseRefsOnNextRefClick = false;
    },
    // This filetree model handling should eventually go.. somewhere.
    _focusTree(fileTicket) {
      const shouldScrollTo = true;
      console.log('focusing', fileTicket);
      const ticketParts = fileTicket.split(':');
      const ticketRepoBranch = ticketParts[0];
      // NOTE(filename-colon): not likely filename contains colon, but restore
      // it in case it does.
      const ticketFilePath = ticketParts.slice(1).join(':');
      let parts = ticketFilePath.split("/");
      // The head part is always the repoBranch.
      parts.unshift(ticketRepoBranch);

      let i = 0;
      let go = (cur, k) => {
        if (cur.isFile) return;
        if (cur.children == null) {
          // Need to make network trip to API, since this part of tree is not
          // yet loaded. Will call back to this function with the node where
          // children are now present.
          this.onLoadMoreTree(cur.id, cur, (c) => go(c,k));
        } else {
          for (let c of cur.children) {
            // console.log(c.name, i, parts[i])
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
            }
          }
        }
      }
      // Need nextTick in continuation, since 'go' potentially sets the
      // recursive model, and this is the first chance for Vue to observe its
      // properties.  So if we want FileTree's watcher to detect the change in
      // highlight, we need to defer that update.
      go(this.nodes, (c) => Vue.nextTick(() => {
        console.log('hiliting filetree node', c.name);
        c.highlight = true;
        if (shouldScrollTo && !this.preventFileTreeScroll) {
          // Did the change propagate already? nextTick to be safe.
          Vue.nextTick(() => scrollToLastHilit());
        }
        this.preventFileTreeScroll = false;
        this._giveBackFocus();
      }));
    },
    // Note: triggered by router changes via __render as wel.
    _loadSource (ticket, mbLineToFocus) {
      console.log('_loadSource');
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
        // See https://github.com/axios/axios/issues/907, argh.
        transformResponse: undefined,
        params: { ticket }
      })
        .then(response => {
          this.setupCodeMirrorHeight();
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
          } else {
            this.$nextTick(() => {
              // TODO keep history state and reuse last viewed line for file
              this._jumpToLine(1);
            });
          }
          this.renderedTicket = ticket;
          this.navigateToFileLineIfNeeded({ ticket, line: mbLineToFocus });
          this.$nextTick(function() {
            console.log('codemirror rendered in', Date.now() - start, Date.now());
          });
          this._focusTree(ticket);

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
        const cmLine = Math.max(0, line - 1);
        addLineClass(this.codemirror, cmLine, "uh-activeline-background");
        const margin = this.codemirror.getScrollInfo().clientHeight;
        this.codemirror.scrollIntoView({
          line: cmLine,
          ch: 0,
        }, /* vertical pixels around */ margin/2);
    },
    navigateToFileLineIfNeeded(routeParams) {
      if (routeParams.ticket) {
        const parts = routeParams.ticket.split('/');
        const lastPart = parts[parts.length - 1];
        this.setWindowTitle(lastPart + (routeParams.line ? (":" + routeParams.line) : "") + " (" + routeParams.ticket + ")");
      }
      const curParams = this.$router.currentRoute.params;
      if (isEqual(curParams, routeParams)) {
        console.log('Preventing duplicate navigation');
        return;
      }
      this.$router.push({
        name: 'file',
        params: routeParams,
      });
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
          console.log('done-markup');
        }
      };
      go();
    },
    __render: function(route) {
      this._loadSource(route.params.ticket, route.params.line);
    },
  },
  computed: {
    mkNavBus () {
      return {
        onClick: this.onNavClick,
        onLoadMoreTree: this.onLoadMoreTree,
      }
    },
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
  beforeRouteUpdate(route, old, next) {
    //console.log('beforeRouteUpdate', route);
    this.__render(route);
    next();
  },
  beforeRouteEnter (to, from, next) {
    // Special for beforeRouteEnter: need next tick to access instance.
    next(vm => {
      //console.log('beforeRouteEnter', to);
      vm.__render(to);
    });
  },
  created () {
    axios.get('/api/filetree')
      .then(response => {
        this.nodes = RH.fileTreeToNav(response.data);
      })
      .catch(err => console.log(err));
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

#header {
  height: 4vh;
}
#ticketDisplay {
  display: flex;
  justify-content: center;
  /* TODO generate theme-fitting style */
  border-bottom: 1px solid rgba(128,128,128,0.5);
}
.top-split {
  width: 100vw;
  /* TODO would need +1 to max out, but then right scrollbar appears? */
  height: 95vh;
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
