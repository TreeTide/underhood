<template>
  <div>
    <LangIcon :for-file="filePath" /> {{filePath}}
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
  },
}
</script>

<style scoped>
</style>
