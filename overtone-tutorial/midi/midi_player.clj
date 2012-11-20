(defrecord Song [ppqn tracks])
(defrecord Track [name events instrument])
(defrecord Note [tick midi-note duration velocity]) ; dur in ticks; created from note on/note off Event pairs

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
         (not= 0 (nth msg 2)))))

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
               (if (java-note-on-event? java-track i)
                 (conj events (note-from-java-note-at java-track i))
                 events))))))

(defn song-from-file
  [file]
  (let [java-seq (javax.sound.midi.MidiSystem/getSequence (java.io.File. file))]
    (->Song (.getResolution java-seq)
            (map java-track->track (.getTracks java-seq)))))
   

(defn play-note
  [start-ms inst note volume]
      (at (+ start-ms (:t note)) (-> (:note note) midi->hz (inst :volume volume))))

(defn play-track
  [start-ms track volume]
  (let [inst (:inst track)]
    (dorun (map #(apply-at (+ start-ms (:t %)) #'play-note start-ms inst % volume) (:notes track)))))

(defn play-song
  [song]
  (let [start-ms (+ (.getTime (java.util.Date.)) 500)
        track-vol (/ 1.0 (count (:tracks song)))]
    (dorun (map #(play-track start-ms % track-vol) (:tracks song)))))
