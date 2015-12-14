(ns midilib)
(def debug nil)

(def key-names '("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))

(def microsecs-per-minute (* 1000000 60))

; This map's keys are status byte high nibbles, and the values are either the
; number of bytes needed (1 or 2) for a channel message or 0 if it's not a
; channel message.
(def num-data-bytes 
     {0x00 0, 0x10 0, 0x20 0, 0x30 0, 0x40 0, 0x50 0, 0x60 0, 0x70 0,
      0x80 2, 0x90 2, 0xa0 2, 0xb0 2, 0xc0 1, 0xd0 1, 0xe0 2, 0xf0 0})

(defstruct midifile-reader
  :handler-fns                    ; If not nil, map of handler functions
  :curr-ticks                     ; Current time, from delta-time in MIDI file
  :ticks-so-far                   ; Number of delta-time ticks so far
  :io                             ; The reader
  :bytes-to-be-read               ; Counts number of bytes expected

  :no-merge                     ; true means continued sysex are not collapsed
  :skip-init                    ; true if initial garbage should be skipped

  :msg-buf

  :midi-sequence
)

(defstruct midi-sequence
  :tracks                               ; Vector
  :ppqn    ; Pulses (i.e. clocks) Per Quarter Note resolution for the sequence
  :format                               ; The MIDI file format (0, 1, or 2)
  :numer
  :denom
  :clocks
  :qnotes
)

(defstruct track
  :instrument                           ; string
  :events                               ; vector
  :channels-used)                       ; vector

(defn make-blank-midi-sequence [] (struct midi-sequence [] 480 1 4 2 24 8))
(defn make-blank-midifile-reader 
 [in handler-fns]
 (struct midifile-reader 
         handler-fns 0 0 in 0 true nil [] (make-blank-midi-sequence)))
(defn make-blank-track [] (struct track "Unnamed" [] []))

(defn note->string 
  "Given a MIDI note number, returns the note name."
  [num]
  (let [note (rem num 12)
        octave (quot num 12)]
    (str (nth key-names note) octave)))

(defn
  #^{:doc "Given a value, return a vector of byte values containing the MIDI
     variable-length value."
     :test (fn                          ; (test #'int->varlen)
            []
            (loop [test-vals '(0x00000000 [0x00]
                               0x00000040 [0x40]
                               0x0000007F [0x7F]
                               0x00000080 [0x81 0x00]
                               0x00002000 [0xC0 0x00]
                               0x00003FFF [0xFF 0x7F]
                               0x00004000 [0x81 0x80 0x00]
                               0x00100000 [0xC0 0x80 0x00]
                               0x001FFFFF [0xFF 0xFF 0x7F]
                               0x00200000 [0x81 0x80 0x80 0x00]
                               0x08000000 [0xC0 0x80 0x80 0x00]
                               0x0FFFFFFF [0xFF 0xFF 0xFF 0x7F])]
              (when-not (nil? test-vals)
                (assert (= (int->varlen (first test-vals)) (second test-vals)))
                (recur (rest (rest test-vals))))))}
  int->varlen
  [val]
  (loop [v (bit-shift-right val 7)
         byte-vec [(bit-and val 0x7f)]]
    (if (zero? v)
      (reverse byte-vec)
      (recur (bit-shift-right v 7)
             (conj byte-vec (+ 0x80 (bit-and v 0x7f)))))))

(defn bpm->mpq
  "Translates beats per minute to microseconds per quarter note (beat)."
  [bpm]
  (/ microsecs-per-minute bpm))

(defn mpq->bpm
  "Translates microseconds per quarter note (beat) to beats per minute."
  [mpq]
  (/ microsecs-per-minute mpq))


(defn error
  "Error handler. Prints error message to *err*."
  [s]
  (.println *err* (str "error: " s))
  (throw (Error. s)))                   ; DEBUG

(defn getc
  "Read a single character from in and return it. Calls error if EOF is seen."
  [mfr]
  (let [c (.read (:io @mfr))]
    (when (neg? c)
      (error "unexpected EOF"))
    (alter mfr assoc :bytes-to-be-read (dec (:bytes-to-be-read @mfr)))
    c))

(defn read16
  "Reads a 16-bit integer from in and returns it."
  [mfr]
  (let [val (+ (bit-shift-left (getc mfr) 8) (getc mfr))]
    (if (zero? (bit-and val 0x8000))
      val
      (- 0 (bit-and val 0x7fff)))))

(defn read32
  "Reads a 32-bit integer from in and returns it."
  [mfr]
  (let [val (+ (bit-shift-left (getc mfr) 24)
               (bit-shift-left (getc mfr) 16)
               (bit-shift-left (getc mfr) 8)
               (getc mfr))]
    (if (zero? (bit-and val 0x80000000))
      val
      (- 0 (bit-and val 0x7fffffff)))))

(defn read-varlen
  "Reads and returns a MIDI varlen value from mfr."
  [mfr]
  (loop [val (getc mfr)]
    (if (zero? (bit-and val 0x80))
      val
      (recur (+ (bit-shift-left (bit-and val 0x7f) 7) (getc mfr))))))

(defn call-handler
  "Calls handler fn if it exists."
  [mfr handler & args]
  (when-let [f (handler (:handler-fns mfr))]
      (apply f mfr args)))

(defn read-mt-header-string
  "Looks for hstr. If not found, calls error. If skip-init is true, keeps
   looking until it is found, else expects hstr to be the next string."
  [hstr skip-init mfr]
  (loop [buf (str (char (getc mfr)) (char (getc mfr)) (char (getc mfr)) (char (getc mfr)))]
    (when-not (= hstr buf)
      (if skip-init
        (recur (str (subs buf 1) (char (getc mfr))))
        (error (str "expected " hstr " but saw " buf))))))

(defn- eat-remaining-bytes
  [mfr]
  (let [nbytes (:bytes-to-be-read @mfr)]
    (println "  eat-remaining-bytes nbytes" nbytes) ; DEBUG
    (when (> nbytes 0)
      (dotimes [_ nbytes] (getc mfr)))))

;;; ================================================================
;;; Channel messages
;;; ================================================================

(defn channel-message
  [mfr running status c1 c2]
  (let [high-nibble (bit-and status 0xf0)
        chan (bit-and status 0x0f)]
    (println "channel-message high-nibble" high-nibble "chan" chan) ; DEBUG
    (cond (= high-nibble NOTE-ON) (call-handler mfr :note-on chan c1 c2)
          (= high-nibble NOTE-OFF) (call-handler mfr :note-off chan c1 c2)
          (= high-nibble POLY-PRESSURE) (call-handler mfr :poly-pressure chan c1 c2)
          (= high-nibble CONTROLLER) (call-handler mfr :controller chan c1 c2)
          (= high-nibble PITCH-BEND) (call-handler mfr :pitch-bend chan c1 c2)
          (= high-nibble PROGRAM-CHANGE) (call-handler mfr :program-change chan c1)
          (= high-nibble CHANNEL-PRESSURE) (call-handler mfr :channel-pressure chan c1)
          true (error (format "illegal chan message 0x%02x" high-nibble)))))

;;; ================================================================
;;; System messages
;;; ================================================================

(defn msg-init
  [mfr]
  (alter mfr assoc :msg-buf []))

(defn msg-read
  [mfr n-bytes]
  (loop [n-bytes n-bytes]
    (when (> n-bytes 0)
      (alter mfr assoc :msg-buf (conj (:msg-buf @mfr) (getc mfr)))
      (recur (dec n-bytes)))))

(defn meta-event
  [mfr]
  (let [type (getc mfr)]
    (msg-init mfr)
    (let [bytes (msg-read mfr (read-varlen mfr))]
      ;; TODO
      (case type
        META-SEQ-NUM nil
        (META-TEXT META-COPYRIGHT META-SEQ-NAME META-INSTRUMENT
          META-LYRIC META-MARKER META-CUE 0x08 0x09 0x0a
          0x0b 0x0c 0x0d 0x0e 0x0f) nil
        META-TRACK-END nil
        META-SET-TEMPO nil
        META-SMPTE nil
        META-TIME-SIG nil
        META-KEY-SIG nil
        META-SEQ-SPECIF nil
        nil)                            ; meta-misc
  )))

(defn msg-add
  [huh]
  ; TODO
  )

(defn handle-sysex
  [msg mfr]
  ; TODO
  )

(defn msg
  [mfr]
  ; TODO
  )

(defn sysex
  [mfr sysex-continue]
  (msg-init mfr)
  (msg-add SYSEX)
  (let [c (msg-read mfr (read-varlen mfr))]
    (if (or (= c EOX) (not (:no-merge @mfr)))
      (handle-sysex (msg mfr))
      (ref-set sysex-continue true))))

(defn handle-arbitrary
  [msg mfr]
  ; TODO
  )

(defn eox
  [mfr sysex-continue]
  (when (not @sysex-continue)
    (msg-init mfr))
  (let [c (msg-read mfr (read-varlen mfr))]
    (cond (not @sysex-continue) (handle-arbitrary (msg mfr))
          (= c EOX) (do
                        (handle-sysex (msg mfr))
                        (ref-set sysex-continue false)))))

(defn system-message
  [mfr c sysex-continue]
  (println "system-message c" c) ; DEBUG
  (cond (= c META-EVENT) (meta-event mfr)
        (= c SYSEX) (sysex mfr sysex-continue)
        (= c EOX) (eox mfr sysex-continue)
        true (error (format "unexpected byte: 0x%02x" c))))

;;; ================================================================
;;; Reading tracks and headers
;;; ================================================================
(defn read-track
  "Read a track. Call f if it's defined."
  [mfr f i]
  (when f (f i))                        ; call user's function if defined
  (read-mt-header-string "MTrk" nil mfr)
  (let [sysex-continue (ref nil)        ; true if last msg was unfinished
        running (ref nil)               ; true when running status used
        status (ref 0)                  ; (possibly running) status byte
        track (make-blank-track)]
    (assoc (:midi-sequence @mfr) :tracks (conj (:tracks midi-sequence) track))
    (alter mfr assoc
           :bytes-to-be-read (read32 mfr) 
           :ticks-so-far 0)
    (while (> (:bytes-to-be-read @mfr) 0)
           (let [curr-ticks (read-varlen mfr)]
             (alter mfr assoc
                    :curr-ticks curr-ticks
                    :ticks-so-far (+ (:ticks-so-far @mfr) curr-ticks))
             (let [c (getc mfr)]
               (when (and @sysex-continue (not= c EOX))
                 (error "didn't find expected continuation of a sysex"))

               (if (zero? (bit-and c 0x80)) ; running status
                 (do
                   (when (zero? @status)
                     (error "unexpected running status"))
                   (ref-set running true))
                 (do
                   (ref-set status c)
                   (ref-set running nil)))

               (let [needed (num-data-bytes (bit-and @status 0xf0))]
                 (if-not (zero? needed) ; i.e., is it a channel message?
                   (let [c1 (if @running c (bit-and (getc mfr) 0x7f))]
                     ; The "& 0x7f" here may seem unnecessary, but I've seen
                     ; "bad" MIDI files that had, for example, volume bytes
                     ; with the upper bit set. This code should not harm
                     ; proper data.
                     (channel-message mfr @running @status c1 (if (> needed 1) (bit-and (getc mfr) 0x7f) 0)))
                   (system-message mfr c sysex-continue))))))
    (println "in read-track eating remaining bytes") ; DEBUG
    (eat-remaining-bytes mfr)
    (call-handler mfr :end-track track)))

(defn read-header
  [mfr]
  (read-mt-header-string "MThd" true mfr)
  (alter mfr assoc :bytes-to-be-read (read32 mfr))
  (let [midi-sequence (:midi-sequence @mfr)
        format (read16 mfr)
        ntrks (read16 mfr)
        division (read16 mfr)]
    (assoc midi-sequence :format format)
    (assoc midi-sequence :ppqn division)
    (eat-remaining-bytes mfr)
    (call-handler mfr :header  (:format midi-sequence) (:ppqn midi-sequence) ntrks)
    ntrks))

; The only public method. Each MIDI event in the file causes a
; method to be called. Returns a midi-sequence structure.
(defn read-from
  [fname handler-fns track-read-fn]
  (with-open [r (java.io.BufferedInputStream. (java.io.FileInputStream. fname))]
    (sync nil
      (let [mfr (ref (make-blank-midifile-reader r handler-fns))
            ntrks (read-header mfr)]
        (when (<= ntrks 0)
          (error "No tracks!"))
        (dotimes [i ntrks] (read-track mfr track-read-fn i))
        (:midi-sequence @mfr)))))
