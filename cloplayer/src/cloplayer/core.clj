(ns cloplayer.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [cloplayer.parser :as parser])
  (:import java.util.Date
           java.lang.Thread
           [java.io BufferedReader InputStreamReader FileNotFoundException]
           java.net.HttpURLConnection)
  (:gen-class))

(def timeout-millis
  "Max time to wait for all agents."
  (* 70 60 1000))                       ; 70 seconds in milliseconds

(def msec-scale-factor
  "Wait this many seconds between requests that have the same timestamp."
  0.008)

(defn sleep-until-the-time-is-right
  "Sleep until it's time to send request. We sleep until the same time as
  passed since the beginning of execution as the request was in the log file
  from the beginning."
  [request-time second-index run-start-time first-request-time]
  (let [req-secs-offset (- request-time first-request-time)
        real-secs-offset (- (.getTime (Date.)) run-start-time)
        delay-msecs (+ (- req-secs-offset real-secs-offset)
                       (* second-index msec-scale-factor))]
    (if (pos? delay-msecs)
      (Thread/sleep delay-msecs))))

(defn read-from-connection
  "Read an HTTP connection. We throw away the response, but make it anyway
  to make sure the server does its thing. If there is an error, we print the
  error."
  [conn]
  (let [in (try
             (BufferedReader. (InputStreamReader. (.getInputStream conn)))
             (catch FileNotFoundException fnfe
               (println "404" (str (.getURL conn))))
             (catch Exception e
               (println (type e) e)))]
    (when in
      (while (.readLine in)
        ;; ignore input line
        ))))

(defn make-http-connection
  "Make an HttpURLConnection to server using request URI, method, user
  agent, and referrer."
  [server request]
  (let [url (io/as-url (str "http://" server (:uri request)))]
    (doto (.openConnection url)
      (.setRequestMethod (str/upper-case (:method request)))
      (.addRequestProperty "User-Agent" (:user-agent request))
      (.addRequestProperty "Referer" (:referrer request))))) ; [sic]

(defn make-http-request
  "Make an HTTP request, send the request to the server, and read (and
  ignore) the response. We read the response to make the server do work."
  [server request]
    (read-from-connection (make-http-connection server request)))

(defn make-request
  "Make a request and return a non-nil value. We need to return a non-nil
  value so that the agent using this function is updated and the code that
  waits for the agent to finish knows we're done.

  The first arg to this function is the current agent state, which we
  ignore."
  [_ server request second-index run-start-time first-request-time]
  (sleep-until-the-time-is-right (:time request) second-index run-start-time first-request-time)
  (make-http-request server request)
  42)                                   ; return any non-nil value

(defn start-request
  "Create and return an agent whose initial value is 0 and whose update
  function has been called. When the function is complete, the agent will
  have a non-zero value."
  [server request second-index run-start-time first-request-time]
  (let [a (agent 0)]
    (send-off a make-request server request second-index run-start-time first-request-time)))

(defn replay-requests
  "Create an agent for each request and return the seq of agents."
  [server requests first-request-time]
  (let [run-start-time (.getTime (Date.))]
    (loop [requests requests
           second-index 0
           prev-request (first requests)
           agents []]
      (if requests
        (recur (next requests)
               (if (= (:time (first requests)) (:time prev-request))
                 (inc second-index)
                 0)
               (first requests)
               (conj agents
                     (start-request server (first requests) second-index
                                    run-start-time first-request-time)))
        agents))))

(defn -main
  "Reads an Apache log file and replays each request to a specified server."
  [file server]
  (let [requests (parser/parse-log-file file)
        first-request-time (:time (first requests))
        agents (replay-requests server requests first-request-time)]
    (doall (map #(await-for timeout-millis %) agents))))
