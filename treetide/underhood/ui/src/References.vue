<template>
  <div class="refPanel">
    <div v-if="ticket">
      <div v-if="!loading">
        <div v-if="_exists(declarations)">
          <div class="refHeading">Declarations</div>
          <!-- TODO un-copy-paste -->
          <div v-for="k in _keys(groupedDeclarations)">
            <div class="refFile">{{k}}</div>
            <div v-for="ref in _values(groupedDeclarations, k)">
              <span class="clickableRef" @click="onClick(ref)"><span class="refLine">{{_refVisualLine(ref)}}</span> <span v-html="_formatRefSnippet(ref)" /></span>
            </div>
          </div>
        </div>

        <div v-if="_exists(definitions)">
          <div class="refHeading">Definition</div>
          <div v-for="k in _keys(groupedDefinitions)">
            <div class="refFile">{{k}}</div>
            <div v-for="ref in _values(groupedDefinitions, k)">
              <span class="clickableRef" @click="onClick(ref)"><span class="refLine">{{_refVisualLine(ref)}}</span> <span v-html="_formatRefSnippet(ref)" /></span>
            </div>
          </div>
        </div>

        <div class="refHeading">References ({{ refCount }})</div>
        <div v-for="k in _keys(groupedRefs)">
          <div class="refFile">
            <!-- TODO choose icon based on language actually -->
            <!-- Hack, haskell not compiled into devicon fonts yet -->
            <img class="tinyIcon" :src='require("devicon/haskell/haskell-original.svg")' />
            <!-- i class="devicon-git-plain"></i -->
            {{k}}
          </div>
          <div v-for="ref in _values(groupedRefs, k)">
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

import 'devicon/devicon.css';

// TODO un-singleton
let state = {
  canceller: null,
};

export default {
  props: {
    ticket: String,
    highlightMode: String,
    highlightStyle: String,
  },
  data () {
    return {
      refCount: 0,
      refs: [],
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
    groupedRefs () {
      return _.mapValues(_.groupBy(this.refs, r => r.sDisplayName),
        vs => _.sortBy(vs, v => this._refVisualLine(v)));
    },
    groupedDefinitions () {
      return _.mapValues(_.groupBy(this.definitions, r => r.sDisplayName),
        vs => _.sortBy(vs, v => this._refVisualLine(v)));
    },
    groupedDeclarations () {
      return _.mapValues(_.groupBy(this.declarations, r => r.sDisplayName),
        vs => _.sortBy(vs, v => this._refVisualLine(v)));
    },
  },
  methods: {
    onClick(r) {
      this.$router.push({
        name: 'file',
        params: {
          ticket: r.sFileTicket,
          line: this._refVisualLine(r),
        },
      });
    },
    _exists(v) {
      return v != null && (v.length == undefined || v.length > 0);
    },
    _keys(o) {
      return _.keys(o);
    },
    _values(rs, k) {
      return rs[k];
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
      return r.sSnippetSpan.from.line + 1;
    },
    _formatRefSnippet(r) {
      // TODO only if single-line span.. or preprocess this on server-side.
      const subStart = r.sSpan.from.ch - r.sSnippetSpan.from.ch;
      const subEnd = r.sSpan.to.ch - r.sSnippetSpan.from.ch;
      const t = r.sSnippet;
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
          this.refs = response.data.refs;
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
  margin-left: 10px;
}
.refLine::after {
  content: ':';
}
.refHeading {
  background: #d8d8d8;
}
.refFile {
  color: #444;
}
.tinyIcon {
  height: 12px;
}
</style>
