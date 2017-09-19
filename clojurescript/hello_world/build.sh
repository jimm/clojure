#!/bin/bash

HERE=$(cd $(dirname $0) && pwd)
java -cp $HOME/lib/cljs.jar:src clojure.main build.clj
