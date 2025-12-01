<template>
  <div class="langIcon">
    <i :class="deviconClass"></i>
  </div>
</template>

<script>
// Note: not using devicon-colors, since it doesn't play together nicely with
// themes.
import 'devicon/devicon-base.css';

export default {
  props: {
    forFile: String,
  },
  data () {
    return {
    }
  },
  computed: {
    iconName() {
      const mapping = {
        hs: "haskell",
        hsc: "haskell",
        cc: "cplusplus",
        cxx: "cplusplus",
        cpp: "cplusplus",
        hpp: "cplusplus",
        h: "c",
        c: "c",
        java: "java",
        js: "javascript",
        go: "go",
        py: "python",
        s: "codeigniter",  // TODO, lacking assembly..
      };
      const parts = this.forFile.split(".");
      if (parts.length == 0) {
        return undefined;
      }
      return mapping[parts[parts.length - 1]];
    },

    deviconClass() {
      return "devicon-" + this.iconName + "-plain tinyIcon";
    },
  },
  methods: {
  },
}
</script>

<style scoped>
.langIcon {
  display: inline-block;
}
.tinyIcon {
  height: 11px;
  font-size: 11px;
}
</style>
