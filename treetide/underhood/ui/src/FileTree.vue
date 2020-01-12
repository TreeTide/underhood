<template>
  <div class="tree">
    <div
      :class="{dir: isDir, label: true, generated: isGenerated}"
      @click="onClick">
      {{ model.name }}
      <span v-if="isDir">[{{ open ? '-' : '+' }}]</span>
    </div>
    <div class="subs" v-show="open" v-if="isDir">
      <file-tree
        v-for="child in model.children"
        :key="child.id"
        :model="child"
        :bus="bus" />
    </div>
  </div>
</template>

<script>

export default {
  name: "file-tree",
  props: {
    model: Object,
    bus: Object,
  },
  data () {
    return {
      open: false,
    }
  },
  computed: {
    isDir () {
      return this.model.children &&
        this.model.children.length
    },
    isGenerated () {
      return this.model.onlyGenerated;
    }
  },
  methods: {
    onClick () {
      if (this.isDir) {
        this.open = !this.open
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
