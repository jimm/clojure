(ns cloplayer.parser
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import java.text.SimpleDateFormat
           java.util.Date
           java.lang.Thread
           [java.io BufferedReader InputStreamReader FileNotFoundException]
           java.net.HttpURLConnection))

(def date-format
  "Format used to parse datetimes in Apache logs."
  (SimpleDateFormat. "dd/MMM/yyyy:HH:mm:ss ZZZZZ"))

(defrecord Request
    [time-str time method uri user-agent referrer])

(defn parse-datetime
  "Given the first part of an Apache log line (IP addresses and datetime),
  return the datetime as a time in milliseconds since the epoch."
  [ip-and-datetime]
  (let [datetime-and-tz (second (str/split ip-and-datetime #"\["))
        date (.parse date-format datetime-and-tz)]
    (.getTime date)))

(defn parse-log-file-line
  "Parses a line from an Apache log file and returns a Request record."
  [line]
  (let [strs (str/split line #"\"")
        num-strs (count strs)
        ip-and-datetime (first strs)
        datetime (first (str/split (second (str/split ip-and-datetime #"\[")) #"\]"))
        method-and-uri (second strs)
        [method, uri] (str/split method-and-uri #" ")
        user-agent (nth strs (- num-strs 2))
        referrer (nth strs (- num-strs 4))]
    (->Request datetime (parse-datetime ip-and-datetime) method uri user-agent referrer)))

(defn parse-log-file
  "Parses an Apache log file and returns a seq of Request records."
  [file]
  (with-open [r (io/reader file)]
    (doall (map parse-log-file-line (line-seq r)))))
