(defn pdelta
  [mfr]
  (print (:curr-ticks @mfr)))

(defn header
  [mfr format division ntrks]
  (println "header: format =" format ", division =" division ", ntrks = " ntrks))

(defn start-track
  [mfr]
  (println "track start"))

(defn end-track
  [mfr track]
  (println "track end"))

(defn note-off
  [mfr chan note vel]
  (pdelta mfr)
  (println "note off chan" chan ", note" note ", vel" vel))

(defn note-on
  [mfr chan note vel]
  (pdelta mfr)
  (if (zero? vel)
    (do
      (print "(note on vel 0) ")
      (note-off mfr chan note 64))
    (println "note on chan" chan ", note" note ", vel" vel)))

(defn pressure
  [mfr chan note press]
  (pdelta mfr)
  (println "pressure chan" chan ", note" note ", press" press))

(defn controller
  [mfr chan control value]
  (pdelta mfr)
  (println "controller chan" chan ", control" control ", value" value))

(defn pitch-bend
  [mfr chan msb lsb]
  (pdelta mfr)
  (println "pitch bend chan" chan ", msb" msb ", lsb" lsb))

(defn program
  [mfr chan program]
  (pdelta mfr)
  (println "program chan" chan ", program" program))

(defn chan-pressure
  [mfr chan press]
  (pdelta mfr)
  (println "chan press chan" chan ", press" press))

(defn sysex
  [mfr msg]
  (pdelta mfr)
  (println "sysex size" (count msg)))

(defn meta-misc
  [mfr type msg]
  (pdelta mfr)
  (println "meta misc type" type ", length" (count msg)))

(defn sequencer-specific
  [mfr type msg]
  (pdelta mfr)
  (println "sequencer specific type" type ", length" (count msg)))

(defn sequence-number
  [mfr num]
  (pdelta mfr)
  (println "sequence number" num))

(defn text
  [mfr type msg]
  (pdelta mfr)
  (println
   (cond (= type midilib/META-SEQ-NAME) ("seq or track name" msg)
         (= type midilib/META-INSTRUMENT) ("instrument name" msg)
         (= type midilib/META-MARKER) ("marker" msg)
         true (str "text = " msg ", type = " type))))

(defn eot
  [mfr]
  (pdelta mfr)
  (println "end of track evenet")
  )

(defn time-signature
  [mfr numer denom clocks qnotes]
  (pdelta mfr)
  (println "time sig numer" numer ", denom" denom ", clocks" clocks ", qnotes" qnotes))

(defn smpte
  [mfr hour min sec frame fract]
  (pdelta mfr)
  (println "smpte" hour ":" min ":" sec ", frame " frame ", fract" fract))

(defn tempo
  [mfr microsecs]
  (pdelta mfr)
  (let [bpm (* (/ 1.0 microsecs)        ; quarter notes per microsecond
               1000000.0                ; quarter notes per second
               60.0)]                   ; quarter notes per minute
    (println "tempo microsecs pqn =" microsecs " (" bpm " bpm)")))

(defn key-signature
  [mfr sharpflat is-minor]
  (pdelta mfr)
  (println "key sig sharpflat" sharpflat ", is-minor" is-minor))

(defn arbitrary
  [mfr msg]
  (pdelta mfr)
  (println "arbitrary length =" (count msg)))

(def mf2text {
     :header header
     :start-track start-track
     :end-track end-track
     :note-on note-on
     :note-off note-off
     :pressure pressure
     :controller controller
     :pitch-bend pitch-bend
     :program program
     :chan-pressure chan-pressure
     :sysex sysex
     :meta-misc meta-misc
     :sequencer-specific sequencer-specific
     :sequence-number sequence-number
     :text text
     :eot eot
     :time-signature time-signature
     :smpte smpte
     :tempo tempo
     :key-signature key-signature
     :arbitrary arbitrary
     })
