;;; https://github.com/overtone/overtone/wiki/Getting-Started

(use 'overtone.live)
(boot-external-server)                  ; does this work? need to click "boot" in SC instead?
(connect-external-server 57110)         ; default port

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
