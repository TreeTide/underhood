<template>
  <div>
    <LangIcon v-if="enableIcon" :for-file="filePath" />
    <span @click="$emit('file-click')">{{filePath}}</span>
    <span 
      v-for="branch in branches"
      class="branch-label uh-background"
      @click="$emit('file-click', branch)"
      >
      b:{{branch}}
    </span>
  </div>
</template>

<script>

import LangIcon from './LangIcon.vue';

export default {
  components: {
    LangIcon,
  },
  props: {
    filePath: String,
    branches: {
      type: Array,  // of String
      default: [],
    },
    enableIcon: {
      type: Boolean,
      default: true,
    },
  },
}
</script>

<style>
.branch-label {
  font-size: x-small;
  padding: 1px;
  border-radius: 2px;
  margin-left: 0.25rem;
  margin-top: -2px;
}
</style>
