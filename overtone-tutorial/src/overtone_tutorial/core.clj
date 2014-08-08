(ns overtone-tutorial.core)

(use 'overtone.live)

;; ================ introduction ================

(definst foo [] (saw 220))
(foo)
(kill foo)

(definst bar [freq 220] (saw freq))
(bar 110)
(kill bar)

(definst baz [freq 440] (* 0.3 (saw freq)))
(baz 220)
(baz 660)
(kill baz)

(foo)
(bar)
(baz)
(stop)

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

;; ================ live coding ================

(definst kick [freq 120 dur 0.3 width 0.5]
  (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
        env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
        sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
        src (sin-osc freq-env)
        drum (+ sqr (* env src))]
    (compander drum drum 0.2 1 0.1 0.01 0.01)))

(kick)

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(c-hat)

(def metro (metronome 128))

(metro)     ; => current beat number
(metro 100) ; => timestamp of 100th beat

(defn player [beat]
  (at (metro beat) (kick))
  (at (metro (+ 0.5 beat)) (c-hat))
  (apply-by (metro (inc beat)) #'player (inc beat) []))

(player (metro))

(metro-bpm metro 120)

(stop) ; or redefine player to not call itself
