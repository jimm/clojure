(ns midilib)

(defrecord Track [name events channels-used sequence]
  (track-add-event [t e] (Track. name (conj events e) channels-used sequence)))
