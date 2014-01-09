;;; Wizard game data. Accessor functions are in wizard.clj.

(def *wizard-allowed-commands [:look :walk :pickup :letgo :inventory :reset])

(def *wizard-nodes
     {:living-room "You are in the living-room. A wizard is snoring loudly on the couch."
      :garden "You are in a beautiful garden. There is a well in front of you."
      :attic "You are in the attic. There is a giant welding torch in the corner."})

(def *wizard-edges
     {:living-room '([:garden "west" "door"]
                     [:attic "upstairs" "ladder"])
      :garden '([:living-room "east" "door"])
      :attic '([:living-room "downstairs" "ladder"])})

(def *wizard-object-locations (ref {:whiskey :living-room
                                    :bucket :living-room
                                    :chain :garden
                                    :frog :garden}))

(def *wizard-location (ref :living-room))
