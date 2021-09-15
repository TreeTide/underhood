<template>
    <div class="topHeader uh-background">
      <span id="spacer1"></span>
      <input id="searchBar" type="text" class="uh-selected-background uh-color" />
      <select id="themeSelect" v-model="theme" class="uh-background uh-color">
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

<style>
  .topHeader {
    display: flex;
    flex-direction: row;
    justify-content: space-between;
    align-items: center;
    /* TODO generate themed style */
    border-bottom: 1px solid grey;
  }
  #searchBar {
    width: 50%;
  }
  #themeSelect {
  }
</style>
