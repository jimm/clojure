NAME = city
CLJS_JAR = $(HOME)/lib/cljs.jar
CLASSPATH = $(CLJS_JAR):src
BUILDFILE = build.clj
SRC_DIR = src/$(NAME)
SRC = $(SRC_DIR)/animation.cljs $(SRC_DIR)/camera.cljs $(SRC_DIR)/context.cljs \
      $(SRC_DIR)/core.cljs $(SRC_DIR)/scene.cljs

.PHONY: all
all:	out/main.js

out/main.js:	$(SRC)
	java -cp $(CLASSPATH) clojure.main $(BUILDFILE)

.PHONY: open
open:	all
	open index.html

.PHONY: clean
clean:
	rm -rf out
