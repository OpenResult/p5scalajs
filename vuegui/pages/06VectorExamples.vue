<template>
<div>
  <div><Nav></div>
  <div id=p5id></div>
  <div id=p5id2></div>
  <div id=p5id3></div>
</div>
</template>

<script>
export default {
  mounted: function () {
    const b = new VectorExamples();
    new p5(b.sketch, "p5id");

    const b2 = new VectorExamples02();
    new p5(b2.sketch, "p5id2");

    const b3 = new VectorExamples03();
    new p5(b3.sketch, "p5id3");
  },
};
</script>

<style scoped>
</style>