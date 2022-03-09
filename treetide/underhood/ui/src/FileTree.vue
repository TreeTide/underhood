<template>
  <div class="tree">
    <div
      :class="{dir: isDir, label: true, 'uh-selected-background': hover }"
      @click="onClick"
      @mouseover="onHover"
      @mouseout="onHoverDone">
      <!-- Note: cm-... arbitrarily picked for generated. Could add indirection here. -->
      <span ref="nameItem" :class="{'cm-string': isGenerated, 'cm-keyword': isHighlight}">{{ model.name }}</span>
      <span v-if="isDir">[{{ isOpen ? '-' : '+' }}]</span>
    </div>
    <div class="subs" v-show="isOpen" v-if="isDir">
      <file-tree
        v-for="child in model.children"
        :key="child.id"
        :model="child"
        :bus="bus" />
    </div>
  </div>
</template>

<script>

let state = {
  hilitModel: null,
  hilitElement: null,
};

export function scrollToLastHilit() {
  if (state.hilitElement != null) {
    state.hilitElement.scrollIntoView({block: "center"});
  }
}

export default {
  name: "file-tree",
  props: {
    model: Object,
    bus: Object,
  },
  data() {
    return {
      hover: false
    }
  },
  computed: {
    isOpen () {
      return this.model.open;
    },
    isDir () {
      return !this.model.isFile;
    },
    canExpand () {
      return this.model.children == null || this.model.children.length > 0;
    },
    isGenerated () {
      return this.model.onlyGenerated;
    },
    isHighlight() {
      return this.model.highlight;
    },
  },
  watch: {
    // Watch the computed item to properly get subprop changes, without
    // installing a too deep watcher.
    'model.highlight': function(v) {
      console.log('hilight-change', v, this);
      if (!v) return;
      // Unhighlight any previous.
      if (state.hilitModel != null) {
        state.hilitModel.highlight = false;
        state.hilitElement = null;
      }
      state.hilitModel = this.model;
      state.hilitElement = this.$refs.nameItem;
    },
  },
  methods: {
    // Note: we use JS instead CSS since we want to reuse the static CodeMirror
    // CSS. Might be doable with SCSS etc, but..
    onHover () {
      this.hover = true;
    },
    onHoverDone() {
      this.hover = false;
    },
    onClick () {
      if (this.isDir) {
        this.$set(this.model, 'open',  !this.model.open);
        let openWhileSingle = (model) => {
          if (!model.open) return;
          if (model.children == null) {
            this.bus.onLoadMoreTree(model.id, model, openWhileSingle);
            return;
          }
          if (model.children.length == 1 && !model.children[0].isFile) {
            let next = model.children[0];
            this.$set(next, 'open', true);
            openWhileSingle(next);
          }
        };
        openWhileSingle(this.model);
      } else {
        this.$set(this.model, 'highlight', true);
        this.bus.onClick(this.model.id)
      }
    },
  },
}
</script>

<style>
.tree {
  cursor: pointer;
  font-family: monospace;
}
.label {
  white-space: nowrap;
}
.subs {
  margin-left: 20px;
}
.dir {
  padding-top: 1px;
  font-weight: normal;
}
</style>
