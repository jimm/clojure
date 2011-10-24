(ns midilib)

(defprotocol Track
  (track-add-event [t e]))

(deftype Track [name events channels-used sequence]
  :as this
  Track
  (track-add-event [e] (Track name) (conj events e) channels-used sequence))
