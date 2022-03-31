<template>
<div>
  <div><Nav></div>
  <div id=p5id></div>
</div>
</template>

<script>
export default {
  mounted: function () {
    const b = new Forces();
    new p5(b.sketch, "p5id");
  },
};
</script>

<style scoped>
</style>