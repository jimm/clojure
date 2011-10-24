; Note: use defrecord instead of deftype

(defprotocol Person
  "A person has a name and can say things."
  (full-name [this] "Returns full name.")
  (speak [this say] "Speaks the given text."))

(defrecord Doctor [fname lname]
  Person
  (full-name [this] (str "Dr. " fname " " lname))
  (speak [this say] (str say))
  ; could have next protocol name plus list of methods here
)
 
(defrecord Cracker [nickname]
  Person
  (full-name [this] nickname)
  (speak [this say] (str "Hey D00d!!1! " say)))

; Use extend for multiple protocols

(def dr (Doctor. "Fred" "Freddums"))
(def ha (Cracker. "leetzy"))

(println (full-name dr))
(println (speak dr "Hello"))

(println (full-name ha))
(println (speak ha "Hello"))

; Add Villan protocol to Cracker. Could have done this all at once above.

(defprotocol Villian
  (do-evil [this]))

(extend Cracker ; or AClass or AnInterface
                ; Defrecord types specified using keyword tags:
                ; ::MyType or :my.ns/MyType
  ; Protocol name
  Villian
  ; Map of new fns or existing fns
  ; Note that these funcs need extra "this" arg
  {:do-evil (fn [this] "MWA-HA-HA")}
  ; more protocols here
)

; Can call extend more than once on same type

(println (do-evil (Cracker. "LucretiaMcEvil")))
