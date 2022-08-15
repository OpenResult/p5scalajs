<template>
<div>
  <div><Nav></div>
  <div class=gameContainer>
    <div id=menu>
      <ul>
        <li v-for="(game, index) in games">
          {{ index }} - {{ game.gid }} - {{ game.gName }}
        </li>
      </ul>
    </div>
    <div id=game>
      <div id=p5id></div>
      <div id=text>{{ text }}</div>
    </div>
    <div id=status>
      status
    </div>
  </div>
</div>
</template>

<script>
export default {
  mounted: function () {
    const b = new Typing(this.textCallback);
    new p5(b.sketch, "p5id");
  },
  data: function () {
    return {
      text: "",
      games: [
        { gid: "gid1", gName: "game 1" },
        { gid: "gid2", gName: "game 2" },
        { gid: "gid3", gName: "game 3" },
        { gid: "gid4", gName: "game 4" },
      ],
    };
  },
  methods: {
    textCallback: function (text) {
      this.text = text;
    },
  },
};
</script>

<style scoped>
.gameContainer {
  display: flex;
}
#text {
  font-family: "Courier New", Courier, monospace;
  font-size: x-large;
  font-weight: bold;
}
</style>