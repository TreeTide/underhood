<template>
  <div class="tree">
    <div
      :class="{dir: isDir, label: true, generated: isGenerated}"
      @click="onClick">
      {{ model.name }}
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

function _isDir(model) {
  return model.children && model.children.length;
}

export default {
  name: "file-tree",
  props: {
    model: Object,
    bus: Object,
  },
  computed: {
    isOpen () {
      return this.model.open;
    },
    isDir () {
      return _isDir(this.model);
    },
    isGenerated () {
      return this.model.onlyGenerated;
    }
  },
  methods: {
    onClick () {
      if (this.isDir) {
        this.$set(this.model, 'open',  !this.model.open);
        if (this.model.open) {
          // Recursively open while there's only a single child dir.
          let curDir = this.model;
          while (curDir.children.length == 1 && _isDir(curDir.children[0])) {
            curDir = curDir.children[0];
            this.$set(curDir, 'open', true);
          }
        }
      } else {
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
.label:hover {
  background: lightgrey;
}
.subs {
  margin-left: 20px;
}
.dir {
  padding-top: 1px;
  font-weight: bold;
}
.generated {
  color: #88a;
}
</style>
