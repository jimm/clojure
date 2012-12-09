;;; ****************************************************************
;;; Data types

(defrecord Song [name ppqn bpm tracks]) ; for now, one tempo per song
(defrecord Track [name events instrument])

;;; Should this protocol be here, or abstracted out so that these MIDI data
;;; record types aren't tied to it so directly, making it more useful for
;;; non-Overtone code?

(defprotocol Playable
  (play [this ti inst track-volume] "Plays event."))

(defrecord Event [tick data]           ; data is vector of bytes
  Playable
  (play [this ti inst track-volume]
        ))

(defrecord Note [tick midi-note duration velocity] ; dur in ticks; created from note on/note off Event pairs
  Playable
  (play [this ti inst track-volume]
    (letfn [(duration-ms [duration-ticks] (/ (* (:msecs-per-tick ti) duration-ticks) 1000))
            (volume [note track-volume]
                         (* track-volume
                            (/ (float (:velocity note)) 127.0)))]
      (at (:t ti) (inst :freq (midi->hz (:midi-note this))
                        :dur (duration-ms (:duration this))
                        :vol (volume this track-volume))))))

(defrecord Controller [tick cc val]
  Playable
  (play [this ti inst track-volume]
        ))

(defrecord TimeInfo [start-ms           ; start millisecs for entire track
                     msecs-per-tick     ; millisecs per tick
                     t])                ; pre-computed time for an event

;;; ****************************************************************
;;; Reading a MIDI file

(defn nth-byte [bytes n] (bit-and (nth bytes n) 0xff))

(defn name-from-java-event [java-track i]
  (let [java-event (.get java-track i)
        msg (.. java-event getMessage getMessage)]
    (and (= 0xff (nth-byte msg 0))
         (= 0x03 (nth-byte msg 1))
         (.trim (String. (byte-array (drop 3 msg)))))))

(defn java-note-on-event? [java-track i]
  (let [java-event (.get java-track i)
        msg (.. java-event getMessage getMessage)]
    (and (= 0x90 (bit-and (nth msg 0) 0xf0))
         (not (zero? (nth msg 2))))))

(defn java-note-off-or-note-on-0-event? [java-track i]
  (let [java-event (.get java-track i)
        msg (.. java-event getMessage getMessage)
        status (bit-and (nth msg 0) 0xf0)]
    (or (= 0x80 status)
        (and (= 0x90 status) (zero? (nth msg 2))))))

(defn java-note-off-matching-event? [java-track i midi-note]
  (let [java-event (.get java-track i)
        msg (.. java-event getMessage getMessage)]
    (or (and (= 0x80 (bit-and (nth msg 0) 0xf0)) ; note off message
             (= midi-note (nth-byte msg 1)))
        (and (= 0x90 (bit-and (nth msg 0) 0xf0)) ; note on message, velocity 0
             (= midi-note (nth-byte msg 1))
             (= 0 (nth msg 2))))))

(defn java-controller-event? [java-track i]
  (let [java-event (.get java-track i)
        msg (.. java-event getMessage getMessage)]
    (= 0xb0 (bit-and (nth msg 0) 0xf0))))

(defn note-from-java-note-at [java-track i]
  (let [java-event (.get java-track i)
        tick (.getTick java-event)
        bytes (.. java-event getMessage getMessage)
        midi-note (nth-byte bytes 1)
        velocity (nth-byte bytes 2)]
    (loop [i (inc i)]
      (if (java-note-off-matching-event? java-track i midi-note)
        (->Note tick midi-note (- (.getTick (.get java-track i)) tick) velocity)
        (recur (inc i))))))

(defn event-from-java-event-at [java-track i]
  (let [java-event (.get java-track i)]
    (->Event (.getTick java-event) (vec (map #(bit-and % 0xff) (.. java-event getMessage getMessage))))))

(defn controller-from-java-controller-at [java-track i]
  (let [java-event (.get java-track i)
        tick (.getTick java-event)
        bytes (.. java-event getMessage getMessage)]
    (->Controller tick (nth-byte bytes 1) (nth-byte bytes 2))))

(defn java-track->track
  [java-track]
  (let [num-events (.size java-track)]
    (loop [i 0
           name nil
           events []]
      (if (>= i num-events)
        (->Track name events nil)
        (recur (inc i)
               (or name (name-from-java-event java-track i))
               (cond (java-note-on-event? java-track i) (conj events (note-from-java-note-at java-track i))
                     (java-note-off-or-note-on-0-event? java-track i) events ; ignore
                     (java-controller-event? java-track i) (conj events (controller-from-java-controller-at java-track i))
                     :else (conj events (event-from-java-event-at java-track i))))))))

(defn track-bpm
  "Given a Track, return the first tempo event translated to BPM. If the
  track has no tempo events, return 120."
  [tempo-track]
  (letfn [(tempo-event? [bytes]
            (and (= 0xff (nth-byte bytes 0))
                 (= 0x51 (nth-byte bytes 1))))]
    (if-let [tempo-event (first (filter #(tempo-event? (:data %)) (:events tempo-track)))]
      (let [bytes (:data tempo-event)
            usecs-per-beat->bpm (fn [upb] (/ 60000000 upb))]
        (usecs-per-beat->bpm
         (+ (bit-shift-left (nth-byte bytes 3) 16)
            (bit-shift-left (nth-byte bytes 4) 8)
                            (nth-byte bytes 5))))
      120)))

(defn song-from-file
  [file]
  (let [java-seq (javax.sound.midi.MidiSystem/getSequence (java.io.File. file))
        tracks (map java-track->track (.getTracks java-seq))]
    (->Song (:name (first tracks))
            (.getResolution java-seq)
            (track-bpm (first tracks))
            tracks)))

;;; TODO write a function that extracts separate notes in a Track
;;; (presumably a drum track) into separate Tracks, perhaps returning a map
;;; from MIDI note or GM drum name or something to track.
;;;
;;; The first track returned contains all non-note events
(defn track-per-note
  "Extracts separate notes in a track (presumably a drum track) into separate
   tracks and returns a seq whose first element is a track containing all
   non-note events and remaining elements are tracks, one per note number."
  [track]
  (let [tname (:name track)
        [nnes nmap] (loop [events (:events track)
                           non-note-events []
                           note-map {}]
                      (let [e (first events)]
                        (cond (nil? events) (list non-note-events note-map)
                              (= (class e) Note) (let [note (:midi-note e)]
                                                   (recur (next events)
                                                          non-note-events
                                                          (assoc note-map note (if (get note-map note)
                                                                                 (conj (get note-map note) e)
                                                                                 (list e)))))
                              :else (recur (next events) (conj non-note-events e) note-map))))]
    (flatten (list (->Track (str tname " " "Non-Note Events") nnes nil)
                   (map #(->Track (str tname " " "Note " (first %) " Events") (second %) nil) nmap)))))

(defn separate-drum-tracks
  "Given a song and a track in that song, replace the track with the tracks
  returned by track-per-note. (If track is not in song this will still
  work.)"
  [song track]
  (let [ts (:tracks song)
        new-tracks (track-per-note track)]
    (assoc song :tracks (flatten (if (some #{track} song)
                                   (replace {track new-tracks} ts)
                                   (conj ts new-tracks))))))

;;; ****************************************************************
;;; Song functions

(defn track-named
  [song name]
  (first (filter #(= (:name %) name) (:tracks song))))

(defn msecs-per-tick
  [song]
  (/ 60000 (:bpm song) (:ppqn song)))

;;; ****************************************************************
;;; Assigning Overtone instruments to tracks

(defn song-with-instruments
  "Returns a song where each track that has a name in track-inst-map is
  assigned to the corresponding instrument."
  [song track-inst-map]
  (assoc song :tracks (map #(assoc % :instrument (get track-inst-map (:name %)))
                           (:tracks song))))

;;; ****************************************************************
;;; Playing a song

(defn abs-time-ms [start-ms msecs-per-tick ticks] (long (+ start-ms (* msecs-per-tick ticks))))

(defn play-track
  [start-ms msecs-per-tick track volume]
  (when-let [inst (:instrument track)]
    (dorun (map #(let [ti (->TimeInfo start-ms msecs-per-tick (abs-time-ms start-ms msecs-per-tick (:tick %)))]
                   (apply-at (:t ti) #'play % ti inst volume nil))
                (:events track)))))

(defn play-song
  [song]
  (let [start-ms (+ (now) 50)
        track-vol (/ 1.0 (count (:tracks song)))]
    (dorun (map #(play-track start-ms (msecs-per-tick song) % track-vol) (:tracks song)))))

;;; ****************************************************************
;;; Example

(comment

(set! *print-length* 10)

(definst foo [freq 440 dur 1.0 vol 1.0]
  (* vol
     (env-gen (perc 0.15 dur) :action FREE)
     (saw freq)))

(def midi-file "/Users/jimm/Documents/Dropbox/Music/Vision Sequences/Equal Rites/At Sea.mid")
(def midi-file "/Users/jimm/Documents/Dropbox/Music/Vision Sequences/Equal Rites/Main Theme.mid")
(def song (song-from-file midi-file))

(map :name (:tracks song))

;; Create a map of track names to the single instrument "foo".
(zipmap (drop 1 (map :name (:tracks song)))
        (repeat foo))

(def swi (song-with-instruments song (zipmap (drop 1 (map :name (:tracks song)))
                                             (repeat foo))))

(play-song swi)

;; TODO write all these instruments. You know: the hard part.
(def midi-file "/Users/jimm/src/github/midilib/examples/NoFences.mid")
(def song (song-from-file midi-file))
(def track-inst-map {"Drums" drumkit
                     "Bass" bass1
                     "Bass Copy" bass2
                     "Piano & Strings" piano-and-strings
                     "Brass" brass
                     "Organ Melody" organ
                     "Piano Solo" piano
                     "Picky Guitar" picky-guitar
                     "Big Jupiter" big-jupiter
                     "Saxes" saxes})
(play-song (song-with-instruments song track-inst-map))
)
