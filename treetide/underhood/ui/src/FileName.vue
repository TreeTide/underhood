<template>
  <div @click="$emit('click', $event)">
    <LangIcon v-if="enableIcon" :for-file="filePath" /> {{filePath}}
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
    enableIcon: {
      type: Boolean,
      default: true,
    },
  },
}
</script>

<style scoped>
</style>
