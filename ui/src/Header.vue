<template>
    <div class="topHeader uh-background">
      <span id="spacer1"></span>
      <input id="searchBar" type="text" class="uh-selected-background uh-selection-background uh-color"
        @keyup.enter="onSearchSubmit"/>
      <span>
        <select
          v-if="availableRepos.length > 1"
          id="repoSelect"
          v-model="selectedRepo"
          class="uh-background uh-color">
          <option :value="null">All repositories</option>
          <option v-for="repo in availableRepos" :key="repo" :value="repo">
            {{ repo }}
          </option>
        </select>

        <!-- Multi-select branch list -->
        <div v-if="filteredBranches.length > 0" id="branchFilterContainer" class="uh-background">
          <div id="branchFilterHeader" class="uh-color">
            <span>Branches ({{ selectedBranches.length }}/{{ filteredBranches.length }})</span>
            <button @click="selectAllBranches" class="branch-btn uh-background uh-color" title="Select All">All</button>
            <button @click="deselectAllBranches" class="branch-btn uh-background uh-color" title="Deselect All">None</button>
            <button @click="toggleBranchList" class="branch-btn uh-background uh-color">{{ branchListExpanded ? '▲' : '▼' }}</button>
          </div>
          <div v-show="branchListExpanded" id="branchList" class="uh-background uh-color">
            <label v-for="branch in filteredBranches" :key="branch" class="branch-item">
              <input
                type="checkbox"
                :value="branch"
                v-model="selectedBranches"
                @change="onBranchSelectionChange"
              />
              <span>{{ branch }}</span>
            </label>
          </div>
        </div>

        <select id="contextLines" v-model="contextLines" class="uh-background uh-color">
          <option v-for="option in contextLineOptions" :key="option.value" :value="option.value">
            {{ option.text }}
          </option>
        </select>
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
      keyMap: 'vim',
      //
      contextLines: 0,
      contextLineOptions: [
        { value: 0, text: "0 ctx" },
        { value: 1, text: "1 ctx" },
        { value: 2, text: "2 ctx" },
        { value: 3, text: "3 ctx" },
        { value: 5, text: "5 ctx" },
        ],
      //
      selectedRepo: null,
      availableRepos: [],
      selectedBranches: [], // Changed from selectedBranch to array
      availableBranches: [],
      repoToBranches: {},
      branchListExpanded: false,
    }
  },
  computed: {
    filteredBranches() {
      if (!this.selectedRepo) {
        return this.availableBranches;
      }
      return this.repoToBranches[this.selectedRepo] || [];
    }
  },
  methods: {
    onSearchSubmit (e) {
      this.$emit('search-bar-text', e.target.value);
    },
    selectAllBranches() {
      this.selectedBranches = [...this.filteredBranches];
      this.onBranchSelectionChange();
    },
    deselectAllBranches() {
      this.selectedBranches = [];
      this.onBranchSelectionChange();
    },
    toggleBranchList() {
      this.branchListExpanded = !this.branchListExpanded;
    },
    onBranchSelectionChange() {
      localStorage.setItem('selectedBranches', JSON.stringify(this.selectedBranches));
      this.bus.onBranchesChange(this.selectedBranches);
    },
    updateAvailableBranches(branchData) {
      console.log('Header.updateAvailableBranches called with:', branchData);
      // branchData format: { repo: [branches], ... }
      this.repoToBranches = branchData || {};

      // Extract unique repos
      this.availableRepos = Object.keys(branchData).sort();
      console.log('availableRepos:', this.availableRepos);

      // Flatten all branches
      this.availableBranches = Object.values(branchData)
        .flat()
        .filter((v, i, a) => a.indexOf(v) === i)
        .sort();

      if (this.availableRepos.length === 0) {
        this.selectedRepo = null;
        this.selectedBranches = [];
        return;
      }

      // Restore saved repo or default to "All repositories" (null)
      const savedRepo = localStorage.getItem('selectedRepo');
      if (savedRepo !== null && savedRepo !== 'null' && this.availableRepos.includes(savedRepo)) {
        this.selectedRepo = savedRepo;
      } else {
        // Default to "All repositories"
        this.selectedRepo = null;
      }

      // Default to all branches selected
      this.selectedBranches = [...this.availableBranches];

      // Restore saved branches if they exist
      const savedBranches = localStorage.getItem('selectedBranches');
      if (savedBranches) {
        try {
          const parsed = JSON.parse(savedBranches);
          // Filter to only include branches that actually exist
          const validBranches = parsed.filter(b => this.availableBranches.includes(b));
          if (validBranches.length > 0) {
            this.selectedBranches = validBranches;
          }
        } catch (e) {
          // Keep default (all branches)
        }
      }

      // Notify App component about the initial branch selection
      this.onBranchSelectionChange();
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
    contextLines (v) {
      this.bus.onContextLines(v);
    },
    selectedRepo(v) {
      if (v) {
        localStorage.setItem('selectedRepo', v);
      } else {
        localStorage.removeItem('selectedRepo');
      }

      // When repo changes, select all branches for new repo
      this.selectedBranches = [...this.filteredBranches];
      this.onBranchSelectionChange();

      this.bus.onRepoChange(v);
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
    width: 35%;
  }
  #repoSelect {
    max-width: 200px;
  }
  #repoSelect:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  /* Branch filter container */
  #branchFilterContainer {
    display: inline-block;
    position: relative;
  }

  #branchFilterHeader {
    padding: 1px 4px;
    cursor: pointer;
    display: flex;
    align-items: center;
    gap: 4px;
  }

  .branch-btn {
    padding: 0px 4px;
    cursor: pointer;
    border: 1px solid;
    background: transparent;
  }

  .branch-btn:hover {
    opacity: 0.8;
  }

  #branchList {
    position: absolute;
    top: 100%;
    left: 0;
    z-index: 1000;
    max-height: 300px;
    overflow-y: auto;
    border: 1px solid;
    min-width: 250px;
    margin-top: 2px;
  }

  .branch-item {
    display: block;
    padding: 4px 8px;
    cursor: pointer;
    white-space: nowrap;
  }

  .branch-item:hover {
    background-color: rgba(255, 255, 255, 0.1);
  }

  .branch-item input[type="checkbox"] {
    margin-right: 8px;
    cursor: pointer;
  }

  #themeSelect {
  }
</style>
