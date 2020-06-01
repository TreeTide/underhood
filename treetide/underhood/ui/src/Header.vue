<template>
    <div>{{currentTicket}}</div>
</template>

<script>
export default {
  props: {
    currentTicket: String,
  },
  data () {
    return {
    }
  },
}
</script>

<style scoped>
  div {
    background-color: lightgreen;
    display: flex;
    justify-content: center;
  }
</style>
