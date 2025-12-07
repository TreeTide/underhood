<template>
  <div :class="_refPanelClasses" ref="topElemRef">
    <div v-if="extLoading">
      Loading refs...
    </div>
    <div v-else>
      <div v-if="ticket || refData">
        <div v-if="!loading">
          <!-- NOTE(kythe-to-zoekt): everything except References is broken now -->
          <div :class="_refHeadingClasses" v-if="_exists(declarations)">Declarations</div>
          <div v-for="ght in groupedDeclarations">
            <div>
              <div :class="_refFileClasses">
                <FileName :file-path="ght.head.sContainingFile.dfDisplayName"
                  class="clickableRef" @file-click="onClick($event, ght.head, ght.head.sSnippets[0])" />
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
            <div class="sectionSpacerBig"/>
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
              <!-- Setting background so themes that have transparent activeline also get a fix backdrop.
                   Not sure this is needed.
              -->
              <div class="refFileWrapper uh-background">
                <div class="refFile uh-selected-background uh-selection-background uh-selected-color">
                  <FileName
                    :file-path="ght.head.sContainingFile.dfDisplayName"
                    :branches="ght.head.sContainingFile.dfBranches"
                    class="clickableRef"
                    @file-click="(ev, br) => onClick(ev, ght.head, ght.head.sSnippets[0], br)" />
                </div>
              </div>
              <div x-note-single v-for="refInfo in capIfNeeded(ght.head.sSnippets)">
                <div class="refSnippet" v-for="ref in refInfo.values">
                  <div class="contextLines linesBefore" v-for="l in ref.linesBefore">
                    <div
                      ><span :class="_refLineClasses">{{_refVisualLine2(ref)}}</span
                      ><span class="lineContent" v-html="_formatLine(l, ght.head.sContainingFile.dfLanguage)"></span
                    ></div>
                  </div>
                  <div class="clickableRef" @click="onClick($event, ght.head, ref)"
                    ><span :class="_refLineClasses">{{_refVisualLine2(ref)}}</span
                    ><span v-html="_formatRefSnippet2(ref, ght.head.sContainingFile.dfLanguage)" 
                  /></div>
                  <div class="contextLines linesAfter" v-for="l in ref.linesAfter">
                    <div
                      ><span :class="_refLineClasses">{{_refVisualLine2(ref)}}</span
                      ><span class="lineContent" v-html="_formatLine(l, ght.head.sContainingFile.dfLanguage)"></span
                    ></div>
                  </div>

                </div>
                <div v-if="refInfo.notShown > 0" class="lineSkips">
                  ... {{ refInfo.notShown }} lines omitted ...
                </div>
              </div>
            </div>
            <div v-if="ght.tail.length > 0" class="sameMatches">
              <div v-for="fileSites in ght.tail">
                <span v-if="fileSites.sDupOfFile">(DUP)</span>
                <span v-else>(SNIP)</span>
                <FileName style="display:inline"
                  :file-path="fileSites.sContainingFile.dfDisplayName"
                  :branches="fileSites.sContainingFile.dfBranches"
                  :enable-icon="false"
                  class="clickableRef"
                  @file-click="(ev, br) => onClick(ev, fileSites, fileSites.sSnippets[0], br)" />
              </div>
            </div>
            <div class="sectionSpacer"/>
          </div>

        </div>
        <div v-else>
          Fetching refs..
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import axios from 'axios';
import he from 'he';

import CodeMirror from 'codemirror';
import 'codemirror/lib/codemirror.css';
import 'codemirror/addon/runmode/runmode.js';
import 'codemirror/mode/go/go.js';

import FileName from './FileName.vue';
import Ticket from './ticket.js';
import Proglang from './proglang.js';

// TODO un-singleton
let state = {
  canceller: null,
};

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
    highlightStyle: String,
    extLoading: Boolean,
    scrollOnClick: Boolean,
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
      // Interaction state
      defaultBranch: null,
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
      return _.mapValues(this.declarations, g => ({
        head: g.sFileSites[0],
        tail: _.tail(g.sFileSites),
      }));
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
      return ['refHeading', 'uh-activeline-background'];
    },
  },
  methods: {
    capIfNeeded (snips) {
      const lineCap = 500  // TODO(configuration)
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

    onClick(e, r, s, mbBranch) {
      console.log("clicky", r, s, mbBranch);
      
      let chosenBranch = mbBranch;
      if (mbBranch == null) {
        // NOTE(default-branch): we remember the last explicitly chosen
        // branch, mark it, and on subsequent branch-less span click we could
        // default to that branch (if available). So user can choose which branch
        // to go for a given span.
        //
        // Though, this is mostly no-op, since only exact-same files get the
        // branch markup on the same instance (with zoekt currently), otherwise
        // they go to SNIP or DUP. Might remove this feat, otherwise might need
        // more integration with search params like branch control.
        let bs = r.sContainingFile.dfBranches ?? [];
        if (this.defaultBranch != null && bs.indexOf(this.defaultBranch) >= 0) {
          chosenBranch = this.defaultBranch;
        } else {
          chosenBranch = bs.length > 0 ? bs[0] : null;
        }
      } else if (this.defaultBranch != mbBranch) {
        this.defaultBranch = mbBranch;
      }
      // NOTE(display-name): there's a bit of special logic in _focusTree that
      // breaks down a file ticket name into parts that correspond with the
      // file tree organization (handle repo name etc).
      // 
      // NOTE(ticket,display-name): for now the file ticket and display coincide.
      // Do we ever have a non-1:1 mapping between these?
      //
      this.bus.onRefClick({
        ticket: Ticket.addBranchToFileTicket(r.sContainingFile.dfFileTicket, chosenBranch),
        line: this._refVisualLine2(s),
      });

      // NOTE: Can we keep the element in focus, after the ref panel collapses
      // back to small size after a top-bar search? Having the vpane move
      // smoothly is a problem, we don't know when to focus.
      // In the meantime, an ugly hack:
      if (this.scrollOnClick) {
        setTimeout(() => {
          e.target.scrollIntoView({
            behavior: 'instant',
            block: 'center',
          });
        }, 250);
      }
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
    _formatRefSnippet(r, progLang) {
      return this._formatRefSnippet2(r.sSnippet, progLang);
    },
    _refVisualLine2(r) {
      return r.snippetOccurrenceSpan.from.line + 1;
    },
    _formatLine(t, progLang) {
      // TODO(cleanup): lot of dups with below
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
      const cmSyntaxMode = Proglang.backendProgLangToCodeMirror(progLang);
      const mode = CodeMirror.getMode(CodeMirror.defaults, cmSyntaxMode);
      const hilit = this._highlight(mode, t);
      return `<span class="cm-s-${mode.name}">` +
        "&nbsp;".repeat(pad2) + hilit + "</span>";
    },
    _formatRefSnippet2(r, progLang) {
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
      // NOTE(syntax-highlight): a file might have subranges using a different
      // language. We don't support that for now, though could ship via snippet
      // eventually.
      const cmSyntaxMode = Proglang.backendProgLangToCodeMirror(progLang);
      const mode = CodeMirror.getMode(CodeMirror.defaults, cmSyntaxMode);
      // NOTE(syntax-highlight): we highlight first, and emphasize after, so
      // the syntax won't break.
      const hilit = this._highlight(mode, t);
      // hilit is a series of spans with class cm-<something>. Let's find which
      // parts we need to highlight.
      const hilitAfterCloses = hilit.split('>');
      const emphLength = subEnd - subStart;
      let i = 0;
      let resParts = [];
      console.log('xyzz', t, hilit, hilitAfterCloses);
      for (const afterClose of hilitAfterCloses) {
        console.log(subStart, subEnd, i, afterClose);
        if (i >= subEnd) {
          resParts.push(afterClose);
          console.log('skip');
          // Ok not to maintain i anymore, we don't need it.
        } else {
          const nextOpenPos = afterClose.indexOf('<');
          const init = he.decode(afterClose.substring(0, nextOpenPos));
          const rest = afterClose.slice(nextOpenPos);
          const ilen = init.length;
          const untilStart = Math.max(0, subStart - i);
          const untilEnd = Math.max(0, subEnd - i);
          if (untilStart < ilen) {
            const pre = init.substr(0, untilStart);
            const mid = init.slice(untilStart, untilEnd);
            const post = init.slice(untilEnd);
            console.log('ok', pre, mid, post);
            resParts.push(he.encode(pre) 
              + "<span class='refPanelHighlight'>" + he.encode(mid) + "</span>"
              + he.encode(post)
              + rest);
          } else {
            console.log('not');
            resParts.push(afterClose);
          }
          i += ilen;
        }
      }
      const hilitEmph = resParts.join('>');
      return `<span class="cm-s-${mode.name}">` +
        "&nbsp;".repeat(pad2) + hilitEmph + "</span>";
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
.sectionSpacerBig {
  margin-bottom: 5px;
}

.refPanelHighlight {
  font-weight: bold;
  text-decoration: underline dotted 2px;
}
.clickableRef {
  cursor: pointer;
}
.clickableRef:hover {
  text-decoration: underline;
}

.refSnippet {
  margin-bottom: 1px;
  padding-bottom: 1px;
  border-bottom: dotted 2px grey;
}
.refSnippet:last-child {
  border-bottom: 0px;
}
.contextLines {
  /*display: none;*/
  /*font-size: smaller;*/
  filter: contrast(0.1);
}

.refLine {
}
.refLine::after {
  content: '|';  /* TODO(robinp): figure if we can make a nice gutter like CM */
}
.refHeading {
  padding-top: 2px;
  padding-bottom: 1px;
  font-weight: bold;
  margin-bottom: 2px;
}
.refFileWrapper {
  z-index: 100;  /* otherwise contrast-filtered lines in firefox would go above */
  margin-top: 2px;
  margin-bottom: 1px;
  position: sticky;
  top: 0rem;
}
.refFile {
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
