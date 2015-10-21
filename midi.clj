;;; See also https://github.com/overtone/midi-clj

(ns midi
  (:import (javax.sound.midi MidiSystem)))

(MidiSystem/getReceiver)

(def song (MidiSystem/getSequence (java.io.File. (str (System/getenv "HOME") "/src/github/midilib/examples/NoFences.mid"))))
(map #(.size %) (.getTracks song))

;;; ================ System ================

(defrecord Device [id device name description vendor type-keyword state])

;;; Loosely copied from
;;; https://github.com/arirusso/midi-jruby/blob/master/lib/midi-jruby/device.rb
(defn all-devices-by-type
  []
  (letfn [(new-device [count device info type] (Device. count device (.getName info) (.getDescription info) (.getVendor info) type {}))]
    (loop [count 0
           devices {:input [], :output []}
           infos (MidiSystem/getMidiDeviceInfo)]
      (if (or (nil? infos) (empty? infos))
        devices
        (let [info (first infos)
              device (MidiSystem/getMidiDevice info)
              new-devices (assoc devices
                            :input (if (not (zero? (.getMaxReceivers device)))
                                     (let [d (new-device count device info :input)]
                                       (assoc devices :input (conj (:input devices) d)))
                                     (:input devices))
                            :output (if (not (zero? (.getMaxTransmitters device)))
                                      (let [d (new-device count device info :output)]
                                        (assoc devices :output (conj (:output devices) d)))
                                      (:output devices)))]
          (recur (inc count) new-devices (next infos)))))))

;;; ================ Input ================

(defn gets
  [d])

(defn gets-str
  [d])

(defn enable-input
  "Returns new copy of device with modified state."
  [d map f]
  ;; TODO
  d)

(defn close-input
  [d])

;;; ================ Output ================

(defmulti puts class)

(defmethod puts String
  [data])

(defmethod puts ::collection
  [data])

(defn enable-output
  [])

(defn close-output
  [device]
  (.close (.device device)))
