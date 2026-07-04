# glwrapper (vendored)

Object-oriented Scala wrapper around OpenGL/LWJGL, vendored from
[krux02/scalagl](https://github.com/krux02/scalagl) (`stable` branch,
commit `961a4eb`/`4e84f40` era, fetched 2026-07-04).

Vendored instead of used as a git-based sbt sub-project because:
- the upstream repo's own `project/plugins.sbt` depends on the same dead
  `sbt-lwjgl-plugin` that blocked the main `downearth` build (see
  `build.sbt` history) -- fixing that build too just to consume ~2700
  lines of code was not worth the extra moving part
- `git://` protocol dependency (used in the original build.scala) no
  longer resolves; GitHub retired anonymous git:// access

No LICENSE file exists upstream; included here with the author's
knowledge/permission (per project owner, 2026-07-04).
