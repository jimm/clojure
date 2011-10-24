(ns midilib)

(defprotocol Sequence)

(deftype Sequence [tracks ppqn format numer denom clocks qnotes]
  :as this
  Sequence)
