<template>
<div>
  <div><Nav></div>
  <div id=p5id></div>
  <div id=status>{{ status }}</div>
  <div>
    todo
    <ul>
      <li>right click mine marker</li>
    </ul>

  </div>
</div>
</template>

<script>
export default {
  data: function() {
    return {
      status: ""
    }
  },
  methods: {
    statusCallback: function(status) {
      this.status = status
    }
  },
  mounted: function () {
    window.b = new Minesweeper(this.statusCallback);
    new p5(window.b.sketch, "p5id");
  }
}
</script>

<style scoped>
</style>