<template>
  <div class="refPanel">
    <div v-if="ticket">
      <div v-if="!loading">
        <div v-if="_exists(declarations)">
          <div class="refHeading">Declarations</div>
          <!-- TODO un-copy-paste -->
          <div v-for="kv in _kvs(groupedDeclarations)">
            <div class="refFile">
              <FileName :file-path="siteDisplayFile(kv)" />
            </div>
            <div v-for="ref in kv.v">
              <span class="clickableRef" @click="onClick(ref)"><span class="refLine">{{_refVisualLine(ref)}}</span> <span v-html="_formatRefSnippet(ref)" /></span>
            </div>
          </div>
          <div class="sectionSpacer"/>
        </div>

        <div v-if="_exists(definitions)">
          <div class="refHeading">Definition</div>
          <div v-for="kv in _kvs(groupedDefinitions)">
            <div class="refFile">
              <FileName :file-path="siteDisplayFile(kv)" />
            </div>
            <div v-for="ref in kv.v">
              <span class="clickableRef" @click="onClick(ref)"><span class="refLine">{{_refVisualLine(ref)}}</span> <span v-html="_formatRefSnippet(ref)" /></span>
            </div>
          </div>
          <div class="sectionSpacer"/>
        </div>

        <div class="refHeading">Callers ({{ callCount }})</div>
        <div v-for="kv in _kvs(groupedCalls)">
          <div class="refFile">
            <FileName :file-path="callDisplayFile(kv)" />
          </div>
          <div v-for="cc in kv.v">
            <span class="clickableRef" @click="onClick(cc.ccContextSite)"><span class="refLine">{{_refVisualLine(cc.ccContextSite)}}</span> <span v-html="_formatRefSnippet(cc.ccContextSite)" /></span>
          </div>
        </div>

        <div class="refHeading">References ({{ refCount }})</div>
        <div v-for="kv in _kvs(groupedRefs)">
          <div class="refFile">
            <FileName :file-path="siteDisplayFile(kv)" />
          </div>
          <div v-for="ref in kv.v">
            <span class="clickableRef" @click="onClick(ref)"><span class="refLine">{{_refVisualLine(ref)}}</span> <span v-html="_formatRefSnippet(ref)" /></span>
          </div>
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
function siteContainerTicket(s) {
  return s.sContainingFile.dfFileTicket;
}

export default {
  props: {
    ticket: String,
    highlightMode: String,
    highlightStyle: String,
  },
  components: {
    FileName
  },
  data () {
    return {
      refCount: 0,
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
    groupedRefs () {
      return _.mapValues(_.groupBy(this.refs, siteContainerTicket),
        vs => _.sortBy(vs, v => this._refVisualLine(v)));
    },
    groupedDefinitions () {
      return _.mapValues(_.groupBy(this.definitions, siteContainerTicket),
        vs => _.sortBy(vs, v => this._refVisualLine(v)));
    },
    groupedDeclarations () {
      return _.mapValues(_.groupBy(this.declarations, siteContainerTicket),
        vs => _.sortBy(vs, v => this._refVisualLine(v)));
    },
  },
  methods: {
    siteDisplayFile(kv) {
      const s = kv.v[0];
      return s.sContainingFile.dfDisplayName;
    },
    callDisplayFile(kv) {
      const c = kv.v[0];
      return c.ccContextSite.sContainingFile.dfDisplayName;
    },

    onClick(r) {
      this.$router.push({
        name: 'file',
        params: {
          ticket: r.sContainingFile.dfFileTicket,
          line: this._refVisualLine(r),
        },
      });
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
    _highlight(l) {
      const mode = CodeMirror.getMode(CodeMirror.defaults, this.highlightMode);
      let res = "";
      CodeMirror.runMode(l, mode, function (t, st) {
        res += `<span class="cm-${st}">${_.escape(t)}</span>`
      });
      return `<span class="cm-s-${this.highlightStyle}">${res}</span>`;
    },
    _refVisualLine(r) {
      return r.sSnippet.snippetOccurrenceSpan.from.line + 1;
    },
    _formatRefSnippet(r) {
      // TODO only if single-line span.. or preprocess this on server-side.
      const fullSpan = r.sSnippet.snippetFullSpan;
      const snippetSpan = r.sSnippet.snippetOccurrenceSpan;
      const subStart = snippetSpan.from.ch - fullSpan.from.ch;
      const subEnd = snippetSpan.to.ch - fullSpan.from.ch;
      const t = r.sSnippet.snippetText;
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
      return "&nbsp;".repeat(pad2) +
        this._highlight(t.substring(0, subStart)) +
        "<span class='refPanelHighlight'>" + this._highlight(t.substring(subStart, subEnd)) + "</span>" +
        this._highlight(t.substring(subEnd));
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
          this.refCount = response.data.refCount;
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
  color: grey;
  margin-left: 0px;
}
.refLine::after {
  content: ':';
}
.refHeading {
  background: #d8d8d8;
}
.refFile {
  margin-top: 3px;
  color: #444;
}
.tinyIcon {
  height: 12px;
}
</style>
