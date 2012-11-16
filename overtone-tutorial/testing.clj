;;; https://github.com/overtone/overtone/wiki/Getting-Started

;;; NOTE: don't need to run SuperCollider because Overtone has the synth
;;; engine packaged as a library, along with minimal JNA interface.

(use 'overtone.live)
;;; The next two are not needed if you're using the the built-in SC server
;;; that comes with Overtone.
;; (boot-external-server)                  ; does this work? need to click "boot" in SC instead?
;; (connect-external-server 57110)         ; default port

(definst foo [] (saw 220))
(foo)
(kill foo)

(definst bar [freq 220] (saw freq))
(bar 110)
(kill bar)

(definst baz [freq 440] (* 0.3 (saw freq)))
(baz 220)
(baz 660)
(kill baz)                              ; kills all bazes

(foo)
(bar)
(baz)
(stop)                                  ; stop all running synths

(definst quux [freq 440] (* 0.3 (saw freq)))
(quux)
(ctl quux :freq 660)
(kill quux)

(definst trem [freq 440 depth 10 rate 6 length 3]
    (* 0.3
       (line:kr 0 1 length FREE)
       (saw (+ freq (* depth (sin-osc:kr rate))))))

(trem)
(kill trem)

(trem 200 60 0.8)
(kill trem)

(trem 60 30 0.2)
(kill trem)

(ctl trem)

(:name trem)
(:params trem)
(map :name (:params trem))

;;; ****************************************************************
;;; multiple channels

(defsynth sin-square [freq 440] 
  (out 0 (* 0.5 (+ (square (* 0.5 freq)) (sin-osc freq))))
  (out 1 (* 0.5 (+ (square (* 0.5 freq)) (sin-osc freq)))))

(sin-square)
(stop)

(defsynth sin-square [freq 440] 
  (out 0 (* [0.5 0.5] (+ (square (* 0.5 freq)) (sin-osc freq)))))

(sin-square [440 443])                  ; works in docs, does not work here (illegal arg)
(stop)

(defsynth sin-square2 [freq 440] 
  (out 0 (* 0.5 [(square (* 0.5 freq)) (sin-osc freq)])))

(sin-square2 440)
(stop)

;;; ****************************************************************
;;; MIDI

;; doesn't work?

(println (midi-in))
(midi-out)

(event-debug-on)
(event-debug-off)

;;; ****************************************************************

;;; Bass note w/3 slightly out-of-tune oscs

(definst thick-bass [freq 55 depth 1 rate 2 length 3]
  (* 0.8
     (line:kr 1 0.4 length FREE)
     (saw freq) ; (+ freq (* depth (sin-osc:kr rate))))
     (saw (+ freq 0.5))
     (saw (- freq 0.5))
;;       (saw (dec freq))
       ;; (saw (+ (inc freq) (* depth (sin-osc:kr (* rate 0.59)))))
       ;; (saw (+ (dec freq) (* depth (sin-osc:kr (* rate 2.01)))))
            )))

(thick-bass)
(kill thick-bass)
