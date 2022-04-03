<template>
  <div id="nav">
    <NuxtLink to="/">Home page</NuxtLink> |
    <NuxtLink to="/01bounce">Bounce</NuxtLink> |
    <NuxtLink to="/02primespiral">PrimeSpiral</NuxtLink> |
    <NuxtLink to="/03minesweeper">Minesweeper</NuxtLink> |
    <NuxtLink to="/04cube">Cube</NuxtLink> |
    <NuxtLink to="/05mandelbrot">Mandelbrot</NuxtLink> |
    <NuxtLink to="/typing">Typing</NuxtLink> |
    <NuxtLink to="/about">About</NuxtLink>
  </div>
</template>

<style>
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
}

#nav {
  padding: 30px;
}

#nav a {
  font-weight: bold;
  color: #2c3e50;
}

#nav a.router-link-exact-active {
  color: #42b983;
}
</style>
