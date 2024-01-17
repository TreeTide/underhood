<template>
  <div class="uh-background uh-color">
    <div v-for="t in enriched">
      Refs {{t.orig.dEdge}} - {{t.orig.dTarget}} span {{t.orig.dSpan}} - {{t.info}}
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

export default {
  name: "TargetSelector",
  props: {
    bus: Object,
    targets: Array,
  },
  components: {
    FileName
  },
  data () {
    return {
      enriched: [],
    }
  },
  computed: {
  },
  methods: {
    onClick(r) {
      this.$router.push({
        name: 'file',
        params: {
          ticket: r.sContainingFile.dfFileTicket,
          line: this._targetSelVisualLine(r),
        },
      });
      // Note: the above router handling could move to app, passing params
      // through the bus too.
      // TODO: dfDisplayName is not the file-tree-mapped name, so can't be
      // directly used to open / highlight the filetree.
      this.bus.onRefClick(r.sContainingFile.dfDisplayName);
    },
  },
  watch: {
    targets(vs) {
      let fetches = [];
      let thiz = this;
      for (let v of vs) {
        let fetch = axios.get('/api/info', {
                params: { ticket: v.dTarget }
              })
        fetches.push(fetch)
      }
      Promise.all(fetches).then(function (rs) {
        let enriched = [];
        for (let i in rs) {
          enriched.push({
            info: rs[i].data,
            orig: vs[i],
          });
        }
        thiz.enriched = enriched;
      }).catch(e => {
        console.log(e)
      })
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
.targetSelPanelHighlight {
  font-weight: bold;
  text-decoration: underline dotted;
}
.clickableRef {
  cursor: pointer;
}
.clickableRef:hover {
  text-decoration: underline;
}
.targetSelHeading {
  padding-top: 2px;
  font-weight: bold;
  margin-bottom: 2px;
}
.targetSelFile {
  margin-top: 2px;
}
</style>
