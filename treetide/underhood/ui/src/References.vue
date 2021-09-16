<template>
  <div :class="_refPanelClasses" ref="topElemRef">
    <div v-if="ticket || refData">
      <div v-if="!loading">
        <!-- NOTE: everything except References is browen now -->
        <div v-if="_exists(declarations)">
          <div :class="_refHeadingClasses">Declarations</div>
          <!-- TODO un-copy-paste -->
          <div v-for="kv in _kvs(groupedDeclarations)">
            <div :class="_refFileClasses">
              <FileName :file-path="siteDisplayFile(kv)" />
            </div>
            <div v-for="ref in kv.v">
              <span class="clickableRef" @click="onClick(ref)"><span :class="_refLineClasses">{{_refVisualLine(ref)}}</span> <span v-html="_formatRefSnippet(ref)" /></span>
            </div>
          </div>
          <div class="sectionSpacer"/>
        </div>

        <div v-if="_exists(definitions)">
          <div :class="_refHeadingClasses">Definition</div>
          <div v-for="kv in _kvs(groupedDefinitions)">
            <div :class="_refFileClasses">
              <FileName :file-path="siteDisplayFile(kv)" />
            </div>
            <div v-for="ref in kv.v">
              <span class="clickableRef" @click="onClick(ref)"><span :class="_refLineClasses">{{_refVisualLine(ref)}}</span> <span v-html="_formatRefSnippet(ref)" /></span>
            </div>
          </div>
          <div class="sectionSpacer"/>
        </div>

        <div :class="_refHeadingClasses" v-if="callCount>0">Callers ({{ callCount }})</div>
        <div v-for="kv in _kvs(groupedCalls)">
          <div :class="_refFileClasses">
            <FileName :file-path="callDisplayFile(kv)" />
          </div>
          <div v-for="cc in kv.v">
            <div class="clickableRef callContext" @click="onClick(cc.ccContextSite)">
              <span :class="_refLineClasses">{{_refVisualLine(cc.ccContextSite)}}</span>
              <span v-html="_formatRefSnippet(cc.ccContextSite)" /></span>
            </div>
            <div v-for="snippet in cc.ccSites">
              <template v-for="callSite in [synthSite(cc.ccContextSite, snippet)]">
                <span class="clickableRef" @click="onClick(callSite)">
                  <span :class="_refLineClasses">{{_refVisualLine(callSite)}}</span>
                  <span v-html="_formatRefSnippet(callSite)" /></span>
                </span>
              </template>
            </div>
          </div>
          <div class="sectionSpacer"/>
        </div>

        <div :class="_refHeadingClasses">References<span v-if="refLineCount>0">: {{ refLineCount }} lines,
            {{ refFileCount }} files<span v-if="refDupFileCount > 0"> ({{ refDupFileCount }} content dups<span v-if="refDupMatchCount > refDupFileCount"> + {{ refDupMatchCount-refDupFileCount }} match dups</span>)</span>
          </span>
        </div>
        <div v-for="ght in groupedRefs">
          <div>
            <div :class="_refFileClasses">
              <FileName :file-path="ght.head.sContainingFile.dfDisplayName"
                class="clickableRef" @click="onClick($event, ght.head, ght.head.sSnippets[0])" />
            </div>
            <div v-for="refInfo in capIfNeeded(ght.head.sSnippets)">
              <div v-for="ref in refInfo.values">
                <div class="clickableRef" @click="onClick($event, ght.head, ref)"><span :class="_refLineClasses">{{_refVisualLine2(ref)}}</span><span v-html="_formatRefSnippet2(ref)" /></div>
              </div>
              <div v-if="refInfo.notShown > 0" class="lineSkips">
                ... {{ refInfo.notShown }} lines omitted ...
              </div>
            </div>
          </div>
          <div v-if="ght.tail.length > 0" class="sameMatches">
            <div v-for="fileSites in ght.tail"
                class="clickableRef"
                @click="onClick($event, fileSites, fileSites.sSnippets[0])">
              <span v-if="fileSites.sDupOfFile">(DUP)</span>
              <span v-else>(SNIP)</span>
              <FileName style="display:inline"
                :file-path="fileSites.sContainingFile.dfDisplayName"
                :enable-icon="false" />
            </div>
          </div>
          <div class="sectionSpacer"/>
        </div>

      </div>
      <div v-else>
        Loading refs..
      </div>
    </div>
  </div>
</template>

<script>
import axios from 'axios';

import CodeMirror from 'codemirror';
import 'codemirror/lib/codemirror.css';
import 'codemirror/addon/runmode/runmode.js';
import 'codemirror/mode/go/go.js';

import FileName from './FileName.vue';

// TODO un-singleton
let state = {
  canceller: null,
};

// Groups by ticket instead display file name, as in odd cases the name can
// collide. For example root="",dir="foo/bar" collides with
// root="foo",dir="bar".
function xxxsiteContainerTicket(s) {
  return xxxs.sContainingFile.dfFileTicket;
}

function _lineColString(p) {
  return p.line + ':' + p.ch;
}

export default {
  props: {
    bus: Object,
    // The ticket to look up references for.
    ticket: String,
    // Refdata passed in directly.
    // TODO: eventually move ticket xref fetch logic outside, and pass that in
    //   as refData as well.
    refData: Object,
    highlightMode: String,
    highlightStyle: String,
  },
  components: {
    FileName
  },
  data () {
    return {
      refLineCount: 0,
      refFileCount: 0,
      refDupFileCount: 0,
      refDupMatchCount: 0,
      callCount: 0,
      refs: [],
      calls: [],
      definitions: [],
      declarations: [],
      // Number of outstanding requests.
      //
      // Using counter instead bool since cancelled requests can mess up a
      // boolean indicator with races, but not the counter.
      loading: 0,
    }
  },
  computed: {
    groupedCalls() {
      return _.groupBy(this.calls, c => siteContainerTicket(c.ccContextSite));
    },
    groupedDefinitions () {
      return _.mapValues(_.groupBy(this.definitions, siteContainerTicket),
        vs => _.sortBy(vs, v => this._refVisualLine(v)));
    },
    groupedDeclarations () {
      return _.mapValues(_.groupBy(this.declarations, siteContainerTicket),
        vs => _.sortBy(vs, v => this._refVisualLine(v)));
    },
    groupedRefs () {
      return _.mapValues(this.refs, g => ({
        head: g.sFileSites[0],
        tail: _.tail(g.sFileSites),
      }));
    },
    _refPanelClasses() {
      return ['uh-background uh-color'];
    },
    _themeClass() {
      return 'cm-s-' + this.highlightStyle;
    },
    _refLineClasses() {
      return ['refLine', 'uh-linenumber'];
    },
    _refHeadingClasses() {
      return ['refHeading', 'uh-selected-background'];
    },
    _refFileClasses() {
      return ['refFile', 'uh-selected-background'];
    },
  },
  methods: {
    capIfNeeded (snips) {
      const lineCap = 500
      if (this.refLineCount > lineCap) {
        const cap = Math.max(2, Math.floor(lineCap / this.refFileCount));
        return [{
          values: _.take(snips, cap),
          notShown: Math.max(0, snips.length - cap)
        }];
      }
      return [{
        values: snips,
        notShown: 0,
      }];
    },
    siteDisplayFile(kv) {
      const s = kv.v[0];
      return s.sContainingFile.dfDisplayName;
    },
    callDisplayFile(kv) {
      const c = kv.v[0];
      return c.ccContextSite.sContainingFile.dfDisplayName;
    },
    synthSite(site, snippet) {
      return {
        sContainingFile: site.sContainingFile,
        sSnippet: snippet,
      };
    },

    onClick(e, r, s) {
      console.log("clicky", r, s);
      // Note: this router handling could move to app, passing params
      // through the bus too.
      this.$router.push({
        name: 'file',
        params: {
          ticket: r.sContainingFile.dfFileTicket,
          line: this._refVisualLine2(s),
        },
      });

      // TODO: dfDisplayName is not the file-tree-mapped name, so can't be
      // directly used to open / highlight the filetree.
      // HACK: replace ":" with "/" for now, which will help zoekt-based
      // tickets to be opened. Still need to find a nicer way.
      // (Actually having to map this is not that bad).
      //
      // TODO: [branch version] Need to pass the name/version of the repo
      // branch as well, so UI can properly identify among multiple repos
      this.bus.onRefClick(r.sContainingFile.dfDisplayName.replace(":", "/"), e);

      // NOTE: Can we keep the element in focus, after the ref panel collapses
      // back to small size after a top-bar search? Having the vpane move
      // smoothly is a problem, we don't know when to focus.
      // In the mean time, an ugly hack:
      setTimeout(() => {
        e.target.scrollIntoView();
      }, 250);
    },
    _exists(v) {
      return v != null && (v.length == undefined || v.length > 0);
    },
    _kvs(o) {
      let res = [];
      for (const k in o) {
        res.push({
          k: k,
          v: o[k]
        });
      }
      return res;
    },
    _highlight(mode, l) {
      let res = "";
      CodeMirror.runMode(l, mode, function (t, st) {
        res += `<span class="cm-${st}">${_.escape(t)}</span>`
      });
      return res;
    },
    _refVisualLine(r) {
      return this._refVisualLine2(r.sSnippet);
    },
    _formatRefSnippet(r) {
      return this._formatRefSnippet2(r.sSnippet);
    },
    _refVisualLine2(r) {
      return r.snippetOccurrenceSpan.from.line + 1;
    },
    _formatRefSnippet2(r) {
      // TODO only if single-line span.. or preprocess this on server-side.
      const fullSpan = r.snippetFullSpan;
      const snippetSpan = r.snippetOccurrenceSpan;
      const subStart = snippetSpan.from.ch - fullSpan.from.ch;
      const subEnd = snippetSpan.to.ch - fullSpan.from.ch;
      const t = r.snippetText;
      const trimmed = _.trimStart(t);
      const pad = t.length - trimmed.length;
      const begin = t.substring(0, pad);
      let pad2 = 0;
      for (let i = 0; i < begin.length; i++) {
        switch (begin[i]) {
          case '\t':
            pad2 += 4;  // TODO config?
            break;
          default:
            pad2 += 1;
            break;
        }
      }
      // TODO(robinp): don't use current highlightMode, rather according to
      //   the ref's languages. Since the two might differ.
      const mode = CodeMirror.getMode(CodeMirror.defaults, this.highlightMode);
      return `<span class="cm-s-${this.highlightStyle}">` +
        "&nbsp;".repeat(pad2) +
        this._highlight(mode, t.substring(0, subStart)) +
        "<span class='refPanelHighlight'>" + this._highlight(mode, t.substring(subStart, subEnd)) + "</span>" +
        this._highlight(mode, t.substring(subEnd)) +
        "</span>";
    },
    _fetchReferences(ticket) {
      if (state.canceller) {
        state.canceller.cancel();
        state.canceller = null;
      }
      state.canceller = axios.CancelToken.source();
      this.loading += 1;
      axios.get('/api/xref', {
        params: { ticket },
        cancelToken: state.canceller.token,
      })
        .then(response => {
          const rc = response.data.refCounts;
          this.refLineCount = rc.rcLines;
          this.refFileCount = rc.rcFiles;
          this.refDupFileCount = rc.rcDupFiles;
          this.refDupMatchCount = rc.rcDupMatches;
          this.callCount = response.data.callCount;
          this.refs = response.data.refs;
          this.calls = response.data.calls;
          this.definitions = response.data.definitions;
          this.declarations = response.data.declarations;
        })
        .catch(err => {
          if (!axios.isCancel(err)){
            console.log(err);
          }
        })
        .then(() => {
          state.canceller = null;
          this.loading -= 1;
        });
    },
  },
  watch: {
    ticket (t) {
      this._fetchReferences(t);
    },
    refData (d) {
      if (d == null) {
        return;
      }
      const rc = d.refCounts;
      this.refLineCount = rc.rcLines;
      this.refFileCount = rc.rcFiles;
      this.refDupFileCount = rc.rcDupFiles;
      this.refDupMatchCount = rc.rcDupMatches;
      this.callCount = d.callCount;
      this.refs = d.refs;
      this.calls = d.calls;
      this.definitions = d.definitions;
      this.declarations = d.declarations;
      this.$refs.topElemRef.scrollIntoView({block: "start"});
    },
  },
  created () {
  },
}
</script>

<style>
.sectionSpacer {
  margin-bottom: 5px;
}
.refPanelHighlight {
  font-weight: bold;
  text-decoration: underline dotted;
}
.clickableRef {
  cursor: pointer;
}
.clickableRef:hover {
  text-decoration: underline;
}
.refLine {
}
.refLine::after {
  content: '|';  /* TODO(robinp): figure if we can make a nice gutter like CM */
}
.refHeading {
  padding-top: 2px;
  font-weight: bold;
  margin-bottom: 2px;
}
.refFile {
  margin-top: 2px;
}
.callContext {
  margin-top: 2px;
  background: #eef;
}
.tinyIcon {
  height: 12px;
}

.lineSkips {
  margin-top: 5px;
  margin-left: 10px;
  font-style: oblique;
}

.sameMatches {
  margin-top: 5px;
  margin-left: 10px;
  font-style: oblique;
}
</style>
