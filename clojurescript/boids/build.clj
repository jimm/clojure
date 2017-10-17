(require 'cljs.build.api)
(cljs.build.api/build
 "src"
 {:main 'boids.core
  :output-to "out/main.js"})
