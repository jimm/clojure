(require 'cljs.build.api)
(cljs.build.api/build
 "src"
 {:main 'graphics.core
  :output-to "out/main.js"})
