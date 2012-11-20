(ns midi
  (:import (javax.sound.midi MidiSystem)))

(MidiSystem/getReceiver)

(def song (javax.sound.midi.MidiSystem/getSequence (File. "/Users/jimm/src/github/clojure/overtone-tutorial/midi/data/at_sea.midi")))
(map #(.size %) (.getTracks song))
