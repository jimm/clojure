;;; ************************************************
;;; https://gist.github.com/647003

;; p. 4
(demo 10 (sin-osc (+ 1000 (* (lf-noise0:kr 12) 600)) 0.3))

;; p. 5
(demo 10
      (rlpf:ar (dust:ar [12 15])
               (+ (* (lf-noise1:ar [1/3, 1/4]) 1500) 1600)
               0.02))

;; p. 6, fig 1.4, but with vars
(demo 60
      (let [l (line:kr :start 1 :end 20 :dur 60)
            i (impulse:kr :freq l)
            vco (t-rand:kr :lo 100 :hi 1000 :trig i)
            vcf (t-rand:kr :lo   1 :hi   10 :trig i)
            vca (linen:kr :gate i :attack-time 0 :sus-level 0.5 :release-time (/ 1 l))]
        (* vca (blip :freq vco :numharm vcf))))
