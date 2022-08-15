# Mill, Cask, Scala 3, Scala.js, P5.js training template
- Template for Open Result trainees to follow Daniel Shiffmans [Nature Of Code](https://www.youtube.com/playlist?list=PLRqwX-V7Uu6ZV4yEcW3uDwOgGXKUUsPOM)
in [Scala.js](https://www.scala-js.org).
- Template only have small part of P5.js facade exposed to Scala.js. This is meant to be extended.
- This template is base on [this](https://github.com/OpenResult/caskscalajs) caskscalajs template.

## Setup
- Requires initial `npm install` in `vuegui` folder
- Develop with `mill -w server.runBackground` (todo add develop mode that does not rebuild vue on each save)

## Links
- p5js reference https://p5js.org/reference/ (note that only few things are in the [facade](js/src/P5.scala) today and needs to be added when required)
- [scalajs](https://www.scala-js.org)

