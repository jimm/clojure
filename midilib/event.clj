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
  :as this
  Note
  Channel
  Event
  (data-as-bytes [] [(+ *NOTE-OFF* chan) note vel])
  (quantize-to [boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (NoteOff new-dt new-tfs chan note vel)))
  (to-s [] (str "Note Off" chan note vel)))

(deftype NoteOn [delta-time time-from-start chan note vel]
  :as this
  Note
  Channel
  Event
  (data-as-bytes [] [(+ *NOTE-ON* chan) note vel])
  (quantize-to [boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (NoteOn new-dt new-tfs chan note vel)))
  (to-s [] (str "Note On" chan note vel)))

(deftype PolyPressure [delta-time time-from-start chan note vel]
  :as this
  Note
  Channel
  Event
  (data-as-bytes [] [(+ *POLY-PRESSURE* chan) note vel])
  (quantize-to [boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (PolyPressure new-dt new-tfs chan note vel)))
  (to-s [] (str "Poly Press" chan note vel)))

(deftype Controller [delta-time time-from-start chan controller value]
  :as this
  Channel
  Event
  (data-as-bytes [] [(+ *CONTROLLER* chan) controller value])
  (quantize-to [boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (Controller new-dt new-tfs chan controller value)))
  (to-s [] (str "Controller" chan controller value)))

(deftype ProgramChange [delta-time time-from-start chan program]
  :as this
  Channel
  Event
  (data-as-bytes [] [(+ *PROGRAM-CHANGE* chan) program])
  (quantize-to [boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (ProgramChange new-dt new-tfs chan program)))
  (to-s [] (str "Prog Chg" chan program)))

(deftype ChannelPressure [delta-time time-from-start chan pressure]
  :as this
  Channel
  Event
  (data-as-bytes [] [(+ *CHANNEL-PRESSURE* chan) pressure])
  (quantize-to [boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (ChannelPressure new-dt new-tfs chan pressure)))
  (to-s [] (str "Chan Press" chan pressure)))

(deftype PitchBend [delta-time time-from-start chan value]
  :as this
  Channel
  Event
  (data-as-bytes [] [(+ *PITCH-BEND* chan) value])
  (quantize-to [boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (PitchBend new-dt new-tfs chan value)))
  (to-s [] (str "Pitch Bend" chan value)))

(defprotocol SystemCommon)

(deftype SystemExclusive [delta-time time-from-start data]
  :as this
  SystemCommon
  Event
  (data-as-bytes [] (concat [*SYSEX*] (to-varlen (count data)) data [*EOX*]))
  (quantize-to [boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (SystemExclusive new-dt new-tfs data)))
  (to-s [] (str "System Exclusive" (count data) "bytes")))

(deftype SongPointer [delta-time time-from-start pointer]
  :as this
  SystemCommon
  Event
  (data-as-bytes [] [*SONG-POINTER* (bit-and (bit-shift-right pointer 8) 0xff) (bit-and pointer 0xff)])
  (quantize-to [boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (SongPointer new-dt new-tfs pointer)))
  (to-s [] (str "Song Pointer" pointer)))

(deftype SongSelect [delta-time time-from-start song]
  :as this
  SystemCommon
  Event
  (data-as-bytes [] [*SONG-SELECT* song])
  (quantize-to [boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (SongSelect new-dt new-tfs song)))
  (to-s [] (str "Song Select" song)))

(deftype TuneRequest [delta-time time-from-start]
  :as this
  SystemCommon
  Event
  (data-as-bytes [] [*TUNE-REQUEST*])
  (quantize-to [boundary]
   (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
     (TuneRequest new-dt new-tfs)))
  (to-s [] "Tune Request"))

(defprotocol Realtime)

(deftype Clock [delta-time time-from-start]
  :as this
  Realtime
  Event
  (data-as-bytes [] [*CLOCK*])
  (quantize-to [boundary]
    (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
      (Clock new-dt new-tfs)))
  (to-s [] "Clock"))

(deftype Start [delta-time time-from-start]
  :as this
  Realtime
  Event
  (data-as-bytes [] [*START*])
  (quantize-to [boundary]
    (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
      (Start new-dt new-tfs)))
  (to-s [] "Start"))

(deftype Continue [delta-time time-from-start]
  :as this
  Realtime
  Event
  (data-as-bytes [] [*CONTINUE*])
  (quantize-to [boundary]
    (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
      (Continue new-dt new-tfs)))
  (to-s [] "Continue"))

(deftype Stop [delta-time time-from-start]
  :as this
  Realtime
  Event
  (data-as-bytes [] [*STOP*])
  (quantize-to [boundary]
    (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
      (Stop new-dt new-tfs)))
  (to-s [] "Stop" ))

(deftype ActiveSense [delta-time time-from-start]
  :as this
  Realtime
  Event
  (data-as-bytes [] [*ACTIVE-SENSE*])
  (quantize-to [boundary]
    (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
      (ActiveSense new-dt new-tfs)))
  (to-s [] "Active Sense"))

(deftype SystemReset [delta-time time-from-start]
  :as this
  Realtime
  Event
  (data-as-bytes [] [*SYSTEM-RESET*])
  (quantize-to [boundary]
    (let [[new-dt new-tfs] (quantize-time-to delta-time time-from-start boundary)]
      (SystemReset new-dt new-tfs)))
  (to-s [] "System Reset"))

(defprotocol MetaEvent)

;; TODO MetaEvent event (distinct from protocol)
;; TODO Marker
;; TODO Tempo
;; TODO TimeSig
;; TODO KeySig
