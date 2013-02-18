(ns midi.core
  (:use [overtone.midi])
  (:gen-class))

(def setup
  {:inputs {:iac-in (midi-in "Bus 1")
            :mb (midi-in "midiboard")
            :ws (midi-in "Wavestation")}
   :outputs {:iac-out (midi-out "IAC Driver Bus 1")
             :unused1 (midi-out "Unitor8/AMT8 Port 1")
             :ws (midi-out "Wavestation")
             :kz (midi-out "K2")
             :px (midi-out "PX")
             :sj (midi-out "SJ")
             :tx2 (midi-out "TX3, TX2")
             :tx3 (midi-out "TX3, TX2")
             :d4 (midi-out "D4")
             :unused8 (midi-out "Unitor8/AMT8 Port 8")}})

(defn in  [key] (get-in setup [:inputs key]))
(defn out [key] (get-in setup [:outputs key]))

;;; Use midi-route to connect input to output.
;;;
;;; See also midi-handle-events, which lets you specify an input and a
;;; function.

(defn one-minute-hookup
  "Hooks up the midiboard to the kz for one minute then shuts everything
  down."
  []
  (let [mb (in :mb)
        kz (out :kz)]
    (midi-route mb kz)                  ; send mb to kz
    (Thread/sleep (* 60 1000))          ; you've got 60 seconds

    (doseq [note (range 128)]           ; Send all-notes-off to :kz
      (midi-note-off kz note))
    (midi-route mb nil)))

(defn pass-through-filter
  "Pass event through to the Kurzweil 100PX."
  [event]
  (midi-send-msg (:receiver (out :px)) (:msg event) -1))

(defn one-minute-filter
  "Hooks up the midiboard to the kz for one minute using midi-handle-events
  then shuts everything down."
  []
  (let [mb (in :mb)
        kz (out :kz)]
    (midi-handle-events mb pass-through-filter)
    (Thread/sleep (* 60 1000))          ; you've got 60 seconds

    (doseq [note (range 128)]           ; Send all-notes-off to :kz
      (midi-note-off kz note))
    (midi-route mb nil)))

(defn close-all
  []
  (doseq [io (list :inputs :outputs)
          inst (vals (io setup))]
    (.close (:device inst))))

(defn -main
  [& args]
  (one-minute-hookup))

(defn testing-thread-interrupt
  []
  (try
    (Thread/sleep (* 60 1000))
    (catch InterruptedException ex (println ex))))
