;;; ****************************************************************
;;; Data types

(defrecord Song [ppqn bpm tracks])      ; for now, one tempo per song
(defrecord Track [name events instrument])
(defrecord Event [tick data])           ; data is vector of bytes
(defrecord Note [tick midi-note duration velocity]) ; dur in ticks; created from note on/note off Event pairs

(defn note? [event] (:midi-note event))

;;; ****************************************************************
;;; Reading a MIDI file

(defn name-from-java-event [java-track i]
  (let [java-event (.get java-track i)
        msg (.. java-event getMessage getMessage)]
    (and (= 0xff (bit-and (nth msg 0) 0xff))
         (= 0x03 (bit-and (nth msg 1) 0xff))
         (.trim (String. (byte-array (drop 3 msg)))))))

(defn java-note-on-event? [java-track i]
  (let [java-event (.get java-track i)
        msg (.. java-event getMessage getMessage)]
    (and (= 0x90 (bit-and (nth msg 0) 0xf0))
         (not (zero? (nth msg 2))))))

(defn java-note-off-matching-event? [java-track i midi-note]
  (let [java-event (.get java-track i)
        msg (.. java-event getMessage getMessage)]
    (or (and (= 0x80 (bit-and (nth msg 0) 0xf0)) ; note off message
             (= midi-note (bit-and (nth msg 1) 0xff)))
        (and (= 0x90 (bit-and (nth msg 0) 0xf0)) ; note on message, velocity 0
             (= midi-note (bit-and (nth msg 1) 0xff))
             (= 0 (nth msg 2))))))

(defn note-from-java-note-at [java-track i]
  (let [java-note-on (.get java-track i)
        tick (.getTick java-note-on)
        bytes (.. java-note-on getMessage getMessage)
        midi-note (bit-and (nth bytes 1) 0xff)
        velocity (bit-and (nth bytes 2) 0xff)]
    (loop [i (inc i)]
      (if (java-note-off-matching-event? java-track i midi-note)
        (->Note tick midi-note (- (.getTick (.get java-track i)) tick) velocity)
        (recur (inc i))))))

(defn event-from-java-event-at [java-track i]
  (let [java-event (.get java-track i)]
    (->Event (.getTick java-event) (vec (map #(bit-and % 0xff) (.. java-event getMessage getMessage))))))

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
               (conj events (if (java-note-on-event? java-track i)
                              (note-from-java-note-at java-track i)
                              (event-from-java-event-at java-track i))))))))

;; first track contains tempo map
(defn song-bpm [java-seq] 120)              ; TODO
  ;; (let [bpm (some (fn [track] (some (????)
  ;;                                   (:events track)))
  ;;                 (:tracks song))]
  ;;   (or bpm 120)))

(defn song-from-file
  [file]
  (let [java-seq (javax.sound.midi.MidiSystem/getSequence (java.io.File. file))
        tracks (map java-track->track (.getTracks java-seq))]
    (->Song (.getResolution java-seq)
            (song-bpm java-seq)
            tracks)))
            

;;; ****************************************************************
;;; Playing a song

;; FIXME duration can't be hard-coded multiplier)
(defn note-duration-ms [duration-ticks] (/ (float duration-ticks) 1000.0))

(defn note-volume [note track-volume]
  (* track-volume
     (/ (float (:velocity note)) 127.0)))

(defmulti play-event (fn [event a b c] (class event)))

(defmethod play-event Note
  [event start-ms inst track-volume]
  (at (+ start-ms (:tick event))
      (inst (midi->hz (:midi-note event))
            (note-duration-ms (:duration event))
            (note-volume event track-volume))))

(defmethod play-event Event
  [event start-ms inst track-volume]
  ;; nothing to do yet
  )

(defn play-track
  [start-ms track volume]
  (let [inst (:instrument track)]
    (dorun (map #(apply-at (+ start-ms (:tick %)) #'play-event % start-ms inst volume nil)
                (:events track)))))

(defn play-song
  [song]
  (let [start-ms (+ (now) 500)
        track-vol (/ 1.0 (count (:tracks song)))]
    (dorun (map #(play-track start-ms % track-vol) (:tracks song)))))

;;; ****************************************************************
;;; Example

(comment
(definst foo [freq 440 dur 1.0 volume 1.0]
  (* volume
     (env-gen (perc 0.15 dur) :action FREE)
     (saw freq)))

(def midi-file "/Users/jimm/Documents/Dropbox/Music/Vision Sequences/Equal Rites/At Sea.mid")
(def midi-file "/Users/jimm/Documents/Dropbox/Music/Vision Sequences/Equal Rites/Main Theme.mid")
(def song (song-from-file midi-file))

(map :name (:tracks song))

;; TODO assign instruments to song; here we use hard-coded foo instrument
(play-song (assoc song :tracks (map #(assoc % :instrument foo) (:tracks song))))
)