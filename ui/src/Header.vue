<template>
    <div class="topHeader uh-background">
      <span id="spacer1"></span>
      <input id="searchBar" type="text" class="uh-selected-background uh-selection-background uh-color"
        @keyup.enter="onSearchSubmit"/>
      <span>
        <select id="keyMapSelect" v-model="keyMap" class="uh-background uh-color">
          <option>sublime</option>
          <option>vim</option>
          <option>emacs</option>
        </select>
        <select id="themeSelect" v-model="theme" class="uh-background uh-color">
          <!-- generated -->
          <option>3024-night</option>
          <option>abbott</option>
          <option>abcdef</option>
          <option>ambiance-mobile</option>
          <option>ambiance</option>
          <option>ayu-dark</option>
          <option>ayu-mirage</option>
          <option>base16-dark</option>
          <option>bespin</option>
          <option>blackboard</option>
          <option>cobalt</option>
          <option>colorforth</option>
          <option>darcula</option>
          <option>dracula</option>
          <option>duotone-dark</option>
          <option>duotone-light</option>
          <option>eclipse</option>
          <option>elegant</option>
          <option>erlang-dark</option>
          <option>gruvbox-dark</option>
          <option>hopscotch</option>
          <option>icecoder</option>
          <option>idea</option>
          <option>isotope</option>
          <option>juejin</option>
          <option>lesser-dark</option>
          <option>liquibyte</option>
          <option>lucario</option>
          <option>material-darker</option>
          <option>material-ocean</option>
          <option>material-palenight</option>
          <option>material</option>
          <option>mbo</option>
          <option>mdn-like</option>
          <option>midnight</option>
          <option>monokai</option>
          <option>neat</option>
          <option>neo</option>
          <option>night</option>
          <option>nord</option>
          <option>oceanic-next</option>
          <option>panda-syntax</option>
          <option>paraiso-dark</option>
          <option>pastel-on-dark</option>
          <option>railscasts</option>
          <option>rubyblue</option>
          <option>seti</option>
          <option>shadowfox</option>
          <option>ssms</option>
          <option>the-matrix</option>
          <option>tomorrow-night-bright</option>
          <option>tomorrow-night-eighties</option>
          <option>ttcn</option>
          <option>twilight</option>
          <option>vibrant-ink</option>
          <option>xq-dark</option>
          <option>xq-light</option>
          <option>yonce</option>
          <option>zenburn</option>
          <!-- special -->
          <option>solarized light</option>
          <option>solarized dark</option>
        </select>
      </span>
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
      keyMap: 'sublime',
    }
  },
  methods: {
    onSearchSubmit (e) {
      this.$emit('search-bar-text', e.target.value);
    },
  },
  watch: {
    theme (v) {
      console.log('theme change', v);
      this.bus.onTheme(v);
    },
    keyMap (v) {
      this.bus.onKeyMap(v);
    },
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
