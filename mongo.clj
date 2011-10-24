(import [com.mongodb Mongo BasicDBObject])
(def mongo (com.mongodb.Mongo.))
(def db (.getDB mongo "clojure"))
(def coll (.getCollection db "test"))

(.drop coll)                            ; drop all data in collection
(dotimes [i 5] (.insert coll [(BasicDBObject. {"a" (inc i)})]))

(println "There are" (.getCount coll) "records in the collection 'test'")

; One way to do a query. Note that we turn the returned results into a seq. If
; you don't do that, you won't see all the records because of the way Clojure
; treats iterable objects in Java.
(loop [i (seq (.find coll))]
  (when i
    (do (println (first i))
        (recur (next i)))))

; Another way to perform a query. Again, we turn the results into a seq.
(dorun (map println (seq (.find coll))))

; Yet another way. We don't have to turn the results into a seq manually;
; doseq seems to do that already.
(doseq [i (.find coll)] (println i))

; And yet another. This time you don't have to turn the results into a seq
; manually because we are calling the cursor object's hasNext and next methods
; directly.
(let [cursor (.find coll)]
  (while (.hasNext cursor)
         (println (.next cursor))))

; Find with select.
(def query (BasicDBObject. {"a" 3}))
(dorun (map println (seq (.find coll query))))
(doseq [i (.find coll query)] (println i))

; List indexes on the collection
(map println (.getIndexInfo coll))
; Create index on the "a" field"
(.createIndex coll (BasicDBObject. {"a" 1}))
(map println (.getIndexInfo coll))

; See if the last operation had an error
(println "Last error :" (.getLastError db))

; see if any previous operation had an error
(println "Previous error :" (.getPreviousError db))

; force an error
(.forceError db)

; See if the last operation had an error
(println "Last error :" (.getLastError db))

(.close mongo)
