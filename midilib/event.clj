(ns midilib)

(defn to-varlen
  [val]
  (loop [buffer [(bit-and val 0x7f)]
         val (bit-shift-right val 7)]
    (if (pos? val)
      (recur (conj buffer (+ 0x80 (bit-and val 0x7f))) (bit-shift-right val 7))
      (reverse buffer))))

(defn quantize-time-to [delta-time time-from-start boundary]
  (let [diff (mod time-from-start boundary)
        new-dt (- delta-time diff)
        new-tfs (- time-from-start diff)]
    (if (>= diff (/ boundary 2))
      [(+ new-dt boundary) (+ new-tfs boundary)]
      [new-dt new-tfs])))

(defn compare-event-times [e1 e2]
  (compare (:time-from-start e1) (:time-from-start e2)))

(defprotocol Event
  (data-as-bytes [e])
  (quantize-to [e boundary])
  (to-s [e]))

(defprotocol Channel)

(defprotocol Note)

(deftype NoteOff [delta-time time-from-start chan note vel]
  Note
  Channel
  Event
  (data-as-bytes [e] [(+ NOTE-OFF chan) note vel])
  (quantize-to [e boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (NoteOff. new-dt new-tfs chan note vel)))
  (to-s [e] (str "Note Off" chan note vel)))

(deftype NoteOn [delta-time time-from-start chan note vel]
  Note
  Channel
  Event
  (data-as-bytes [e] [(+ NOTE-ON chan) note vel])
  (quantize-to [e boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (NoteOn. new-dt new-tfs chan note vel)))
  (to-s [e] (str "Note On" chan note vel)))

(deftype PolyPressure [delta-time time-from-start chan note vel]
  Note
  Channel
  Event
  (data-as-bytes [e] [(+ POLY-PRESSURE chan) note vel])
  (quantize-to [e boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (PolyPressure. new-dt new-tfs chan note vel)))
  (to-s [e] (str "Poly Press" chan note vel)))

(deftype Controller [delta-time time-from-start chan controller value]
  Channel
  Event
  (data-as-bytes [e] [(+ CONTROLLER chan) controller value])
  (quantize-to [e boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (Controller. new-dt new-tfs chan controller value)))
  (to-s [e] (str "Controller" chan controller value)))

(deftype ProgramChange [delta-time time-from-start chan program]
  Channel
  Event
  (data-as-bytes [e] [(+ PROGRAM-CHANGE chan) program])
  (quantize-to [e boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (ProgramChange. new-dt new-tfs chan program)))
  (to-s [e] (str "Prog Chg" chan program)))

(deftype ChannelPressure [delta-time time-from-start chan pressure]
  Channel
  Event
  (data-as-bytes [e] [(+ CHANNEL-PRESSURE chan) pressure])
  (quantize-to [e boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (ChannelPressure. new-dt new-tfs chan pressure)))
  (to-s [e] (str "Chan Press" chan pressure)))

(deftype PitchBend [delta-time time-from-start chan value]
  Channel
  Event
  (data-as-bytes [e] [(+ PITCH-BEND chan) value])
  (quantize-to [e boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (PitchBend. new-dt new-tfs chan value)))
  (to-s [e] (str "Pitch Bend" chan value)))

(defprotocol SystemCommon)

(deftype SystemExclusive [delta-time time-from-start data]
  SystemCommon
  Event
  (data-as-bytes [e] (concat [SYSEX] (to-varlen (count data)) data [EOX]))
  (quantize-to [e boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (SystemExclusive. new-dt new-tfs data)))
  (to-s [e] (str "System Exclusive" (count data) "bytes")))

(deftype SongPointer [delta-time time-from-start pointer]
  SystemCommon
  Event
  (data-as-bytes [e] [SONG-POINTER (bit-and (bit-shift-right pointer 8) 0xff) (bit-and pointer 0xff)])
  (quantize-to [e boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (SongPointer. new-dt new-tfs pointer)))
  (to-s [e] (str "Song Pointer" pointer)))

(deftype SongSelect [delta-time time-from-start song]
  SystemCommon
  Event
  (data-as-bytes [e] [SONG-SELECT song])
  (quantize-to [e boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (SongSelect. new-dt new-tfs song)))
  (to-s [e] (str "Song Select" song)))

(deftype TuneRequest [delta-time time-from-start]
  SystemCommon
  Event
  (data-as-bytes [e] [TUNE-REQUEST])
  (quantize-to [e boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (TuneRequest. new-dt new-tfs)))
  (to-s [e] "Tune Request"))

(defprotocol Realtime)

(deftype Clock [delta-time time-from-start]
  Realtime
  Event
  (data-as-bytes [e] [CLOCK])
  (quantize-to [e boundary]
    (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
      (Clock. new-dt new-tfs)))
  (to-s [e] "Clock"))

(deftype Start [delta-time time-from-start]
  Realtime
  Event
  (data-as-bytes [e] [START])
  (quantize-to [e boundary]
    (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
      (Start. new-dt new-tfs)))
  (to-s [e] "Start"))

(deftype Continue [delta-time time-from-start]
  Realtime
  Event
  (data-as-bytes [e] [CONTINUE])
  (quantize-to [e boundary]
    (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
      (Continue. new-dt new-tfs)))
  (to-s [e] "Continue"))

(deftype Stop [delta-time time-from-start]
  Realtime
  Event
  (data-as-bytes [e] [STOP])
  (quantize-to [e boundary]
    (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
      (Stop. new-dt new-tfs)))
  (to-s [e] "Stop" ))

(deftype ActiveSense [delta-time time-from-start]
  Realtime
  Event
  (data-as-bytes [e] [ACTIVE-SENSE])
  (quantize-to [e boundary]
    (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
      (ActiveSense. new-dt new-tfs)))
  (to-s [e] "Active Sense"))

(deftype SystemReset [delta-time time-from-start]
  Realtime
  Event
  (data-as-bytes [e] [SYSTEM-RESET])
  (quantize-to [e boundary]
    (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
      (SystemReset. new-dt new-tfs)))
  (to-s [e] "System Reset"))

(defprotocol MetaEvent)

;; TODO MetaEvent event (distinct from protocol)
;; TODO Marker
;; TODO Tempo
;; TODO TimeSig
;; TODO KeySig
