<template>
    <div class="uh-background">
      <div><span class="cm-keyword">{{currentTicket}}</span></div>
      <select v-model="theme">
        <option>zenburn</option>
        <option>monokai</option>
        <option>idea</option>
        <option>night</option>
        <option>solarized light</option>
        <option>solarized dark</option>
        <option>the-matrix</option>
        <option>darcula</option>
      </select>
    </div>
</template>

<script>
export default {
  props: {
    currentTicket: String,
    bus: Object,
  },
  data () {
    return {
      theme: 'zenburn',
    }
  },
  watch: {
    theme (v) {
      console.log('theme change', v);
      this.bus.onTheme(v);
    }
  },
}
</script>

<style scoped>
  div {
    display: flex;
    justify-content: center;
  }
</style>
