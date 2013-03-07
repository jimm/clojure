(ns midi.core
  (:use [overtone.midi]
        [overtone.at-at])
  (:gen-class))

;;; ================ MIDI setup ================

(defn load-setup
  "This is a function, because when it's compiled the gear might not be
  attached."
  []
  {:inputs {:iac-in (midi-in "Bus 1")
            :mb (midi-in "midiboard")
            :ws (midi-in "Wavestation")}
   :outputs {:iac-out (midi-out "IAC Driver Bus 1")
             :ws (midi-out "Wavestation")
             :kz (midi-out "K2")
             :px (midi-out "PX")
             :sj (midi-out "SJ")
             :tx2 (midi-out "TX3, TX2")
             :tx3 (midi-out "TX3, TX2")
             :d4 (midi-out "D4")}})

;;; The unused input and output ports have names of the form
;;; "Unitor8/AMT8 Port N" where N is 1-8.

(def setup (memoize load-setup))

(defn in  [key] (get-in (setup) [:inputs key]))
(defn out [key] (get-in (setup) [:outputs key]))

;;; ================ helpers ================

(def my-pool (mk-pool))                 ; Used by overtone.at-at functions

(defn cleanup-connection
  [in out]
  (doseq [note (range 128)] (midi-note-off out note))
  (midi-route in nil))

;;; ================ routing ================

;; ==== use midi-route ====

(defn one-minute-hookup
  "Hooks up the midiboard to the kz using midi-route for one minute then
  turns off all notes and disconnects the route."
  []
  (let [mb (in :mb), kz (out :kz)]
    (midi-route mb kz)
    (after (* 60 1000) #(cleanup-connection mb kz) my-pool)))

;; ==== use midi-handle-events ====

(defn pass-through-filter
  "Pass event through to the Kurzweil 100PX."
  [event]
  (midi-send-msg (:receiver (out :px)) (:msg event) -1))

(defn one-minute-filter
  "Hooks up the midiboard to the kz using midi-handle-events for one minute
  then turns off all notes and disconnects the route."
  []
  (let [mb (in :mb), kz (out :kz)]
    (midi-handle-events mb pass-through-filter)
    (after (* 60 1000) #(cleanup-connection mb kz))))

;;; ================ cleanup ================

(defn close-all
  "Send .close to all inputs and outputs."
  []
  (doseq [io (list :inputs :outputs)
          inst (vals (io setup))]
    (.close (:device inst))))

;;; ================ main ================

(defn -main
  [& args]
  (one-minute-hookup)
  (close-all))
