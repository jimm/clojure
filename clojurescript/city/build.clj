(require 'cljs.build.api)
(cljs.build.api/build
 "src"
 {:main 'city.core
  :output-to "out/main.js"})
