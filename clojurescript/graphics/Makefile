CLJS_JAR = $(HOME)/lib/cljs.jar
CLASSPATH = $(CLJS_JAR):src
BUILDFILE = build.clj

.PHONY: all
all:	out/main.js

out/main.js:	src/graphics/core.cljs
	java -cp $(CLASSPATH) clojure.main $(BUILDFILE)

.PHONY: open
open:	all
	open index.html
