<template>
  <div class="langIcon">
    <!-- Hack, haskell not compiled into devicon fonts yet -->
    <span v-if="isHaskell">
      <img class="tinyIcon" :src='require("devicon/haskell/haskell-original.svg")' />
    </span>
    <i v-else :class="deviconClass"></i>
  </div>
</template>

<script>
import 'devicon/devicon.css';
import 'devicon/devicon-colors.css';

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
      };
      const parts = this.forFile.split(".");
      if (parts.length == 0) {
        return undefined;
      }
      return mapping[parts[parts.length - 1]];
    },

    isHaskell() {
      return this.iconName == "haskell";
    },

    deviconClass() {
      return "devicon-" + this.iconName + "-plain colored tinyIcon";
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
