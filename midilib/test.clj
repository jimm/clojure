(ns midilib)
(load-file "consts.clj")
(load-file "event.clj")
(load-file "track.clj")
(load-file "sequence.clj")
(load-file "midifile.clj")
(load-file "mf2text.clj")
(midilib/read-from
 (str (System/getenv "dbox") "/Music/MIDI files/NoFences.mid")
 mf2text
 #(println "loading track" %))
