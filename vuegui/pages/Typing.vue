<template>
<div>
  <div><Nav></div>
  <div id=p5id></div>
  <div id=text>{{ text }}</div>
</div>
</template>

<script>
export default {
  mounted: function () {
    const b = new Typing(this.textCallback);
    new p5(b.sketch, "p5id");
  },
  data: function() {
    return {
      text: ""
    }
  },
  methods: {
    textCallback: function(text) {
      this.text = text
    }
  },
};
</script>

<style scoped>
#text {
  font-family: 'Courier New', Courier, monospace;
  font-size: x-large;
  font-weight: bold;
}
</style>