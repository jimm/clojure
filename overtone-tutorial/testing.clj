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
;; ctl lets you change parameters of one or more running synths. (For more
;; than one pass in a vector of synth ids, or if you've used definst then
;; you can use the instrument name which represents the group of all synths
;; running under that name).
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
;;; from the mailing list, modified to bring up to date

(definst example1 []
  (let [n-exciters 10
        n-delays 7
        n-allpasses 4
        mix #(apply + %)
        exciter (fn [] (resonz
                        (* 50 (dust 0.2))
                        (+ 200 (rand-int 3000))
                        0.05))
        noisy-delay (fn [in] (let [noiz (+ 0.05
                                           (* 0.04 (lf-noise1:kr (clojure.core/rand 0.1))))]
                               (comb-l in 0.1 noiz 15)))
        exciters (mix (take n-exciters (repeatedly exciter)))
        delays (mix (take n-delays (repeatedly #(noisy-delay exciters))))
        reverb (delay-n exciters 0.048)
        out (reduce (fn [in _] (allpass-n in 0.05 [(clojure.core/rand 0.05)
                                                   (clojure.core/rand 0.05)] 1))
                    delays
                    (range n-allpasses))]
    (+ reverb out)))

(do
  (example1)
  (at (+ (.getTime (java.util.Date.)) 10000) (kill example1)))

;; ---
;; had to change :free to FREE

(definst bong [note 60 velocity 50]
  (let [freq (midicps note)
        src (+ (sin-osc freq)
               (* 0.5 (sin-osc (* 2.1 freq)))
               (* 0.25 (sin-osc (* 4.9 freq)))
               (* 0.2 (sin-osc (* 7.3 freq)))
               (* 0.05 (sin-osc (* 11.2 freq)))
               (* 0.1 (triangle (* 8.8 freq)))
               (* 0.1 (triangle (* 8.8 freq)))
               (* 0.1 (square (* 1.3 freq)))
               (* 0.1 (triangle (* 2.8 freq)))
               (* 0.1 (square (* 4.2 freq))))
        env (env-gen (perc 0.01 5) :action FREE)]
    (* (/ velocity 128) src env)))

;;; See also
;;; - defcgen

;;; ----
;;; from auto_dubstep.clj

(definst dubby [bpm 120]
      (let [notes   [40 41 28 28 28 27 25 35 78]
            trig    (impulse:kr (/ bpm 120))
            freq    (midicps (lag (demand trig 0 (dxrand notes INF)) 0.25))
            swr     (demand trig 0 (dseq [1 6 6 2 1 2 4 8 3 3] INF))
            sweep   (lin-exp (lf-tri swr) -1 1 40 3000)
            wob     (mix (saw (* freq [0.99 1.01])))
            wob     (lpf wob sweep)
            wob     (* 0.8 (normalizer wob))
            wob     (+ wob (bpf wob 1500 2))
            wob     (+ wob (* 0.2 (g-verb wob 9 0.7 0.7)))

            kickenv (decay (t2a (demand (impulse:kr (/ bpm 30)) 0 (dseq [1 0 0 0 0 0 1 0 1 0 0 1 0 0 0 0] INF))) 0.7)
            kick    (* (* kickenv 7) (sin-osc (+ 40 (* kickenv kickenv kickenv 200))))
            kick    (clip2 kick 1)

            snare   (* 3 (pink-noise [1 1]) (apply + (* (decay (impulse (/ bpm 240) 0.5) [0.4 2]) [1 0.05])))
            snare   (+ snare (bpf (* 4 snare) 2000))
            snare   (clip2 snare 1)]

        (clip2 (+ wob kick snare) 1)))

(dubby)
(ctl dubby :bpm 240)
;; want to figure out how to modify kick array
(kill dubby)

