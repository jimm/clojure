(ns ircbot)

(def irc-sigs (ref nil))
(def irc-sigs-count (ref 0))

(def *default-settings* {
     :server "irc.freenode.net"
     :port 6667
     :nick "jimm-test-bot"
     :channel "#jimm-test"
     :full-name "Jimm Bot"
     :sigfile (str (System/getenv "pim") "/signatures")})

(defn irc-send
  "Send a message to the IRC server and print it to the screen."
  [out message]
  (println (str "--> " message))
  (.print out (str message "\r\n")))

(defn irc-connections
  "Create a connection to the server. Returns a vector [in-stream out-stream]."
  [host port]
  (let [s (java.net.Socket. host port)]
    (.setSoTimeout s 0)
    [(java.io.BufferedReader. (java.io.InputStreamReader. (.getInputStream s)))
     (java.io.PrintWriter. (.getOutputStream s))]))

(defn irc-signature
  [file]
  (dosync
   (when-not @irc-sigs
     (let [sigs (.split (slurp file) "\\n\\n")]
       (ref-set irc-sigs sigs)
       (ref-set irc-sigs-count (count sigs))))
   (nth @irc-sigs (rand-int @irc-sigs-count))))

(defn irc-message-to-me
  [settings out from user host to text]
  (let [reply-to (if (= (:nick settings) to) to from)
        words (.split text "[ \\t]")
        verb (.toLowerCase (first words))]
    (cond
     (= "hello" verb)
       (irc-send out (str "PRIVMSG " reply-to " :Hello to you too, " from "!"))
     (= "quit" verb)
       (irc-send out "QUIT :Goodbye")
     (or (= "sig" verb) (= "signature" verb))
       (irc-send out (str "PRIVMSG " reply-to " :" (.replaceAll (irc-signature (:sigfile settings)) "[\\r\\n]" " ")))
     true
       (println text))))

(defn irc-privmsg
  [settings out from user host to text]
  (cond
   (.startsWith text "\001PING ")
     (irc-send out (str "NOTICE " from " :\001PING " (.substring text 6)))
   (.startsWith text "\001VERSION")
     (irc-send out (str "NOTICE " from" :\001VERSION Clojure-irc v0.0.1\001"))
   (.startsWith text (str (:nick settings) ":"))
     (irc-message-to-me from user host to (.substring text (+ (count (:nick settings)) 2)))
   true
     (println text)))

(defn handle-server-input
  [settings out s]
  (let [input (.trim s)]
  (cond
   (.startsWith input "PING :") 
     (irc-send out (str "PONG :" (.substring input 6)))
   (.contains input "PRIVMSG")
     (let [matches (re-seq #"^:(.+?)!(.+?)@(.+?)\sPRIVMSG\s(\S+)\s:(.*)" input)]
       (apply irc-privmsg settings out (drop 1 (first matches))))
   (.matches input "^:(.+?)!(.+?)@(.+?)\\s(\\d+)\\s(\\S+)\\s(.*)") ; FIXME shouldn't be matching this twice
     (let [matches (re-seq #"^:(.+?)!(.+?)@(.+?)\s(\d+)\s(\S+)\s(.*)" input)
           ; from user host code to msg
           code (nth (drop 1 (first matches)) 4)]
       (println "[ Error" code "]"))
   true
     (println s))))

(defn irc-main-loop
  [settings out]
  (when-let [line (read-line)]
    (handle-server-input settings out line)
    (recur settings out)))

(defn irc-connect
  [{:keys [server port nick full-name channel]}]
  (let [[in out] (irc-connections server port)]
    (irc-send out (str "USER " nick " dummy-host dummy-server :" full-name))
    (irc-send out (str "NICK " nick))
    (irc-send out (str "JOIN " channel))
    [in out]))

(defn irc-bot
  [settings]
  (let [[in out] (irc-connect settings)]
    (binding [*in* in]
      (irc-main-loop settings out))))
