(ns robot-name
  "Managing robot factory settings."
  {:author "Eric Bailey"}
  (:require [clojure.core.typed :refer [Atom1 IFn Int Map U
                                        ann defalias]]
            [clojure.pprint :refer [cl-format]])
  (:import (java.util Random)))

;;;; == TYPE ALIASES ===========================================================

(defalias Robot     '{:name String})
(defalias RobotAtom (Atom1 Robot))
(defalias RobotName String)


;;;; == ANNOTATION HACKS =======================================================

;; NOTE: This is NOT universally valid, but it works for robot-name.
(ann ^:no-check clojure.core/not-empty [String -> (U nil String)])

;; NOTE: This is NOT universally valid, but it works for robot-name.
(ann ^:no-check clojure.pprint/cl-format
     [nil String Character Character Int -> String])


;;;; == PRIVATE API ============================================================

(ann nameless-robot Robot)
(def ^:private nameless-robot
  "A map representing a robot with the name, \"\"."
  {:name ""})

(ann random Random)
(def ^:private random
  "A random number generator."
  (Random.))

(ann capital-letters String)
(def ^:private capital-letters
  "A sequence of the capital letters, A-Z."
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(ann rand-capital-letter [-> Character])
(defn- rand-capital-letter
  "Return a random capital letter."
  []
  (rand-nth capital-letters))

(ann rand-number (IFn [-> Int]
                      [Random -> Int]))
(defn- rand-number
  "Return a random integer from 0 through 999."
  ([]          (rand-number random))
  ([^Random r] (. r (nextInt 1000))))

(ann rand-name [-> RobotName])
(defn- rand-name
  "Return a random robot name of the form \"AA000\"."
  []
  (cl-format nil "~A~A~3,'0d"
             (rand-capital-letter)
             (rand-capital-letter)
             (rand-number)))


;;;; == PUBLIC API =============================================================

(ann robot [-> RobotAtom])
(defn robot
  "Create and return a new, nameless robot."
  []
  (atom nameless-robot))

(ann robot-name [RobotAtom -> RobotName])
(defn robot-name
  "Given a robot, if it has a name, return it,
  otherwise assign it a new name and return that."
  [robot]
  (or (not-empty (:name @robot))
      (:name (swap! robot assoc :name (rand-name)))))

(ann reset-name [RobotAtom -> Robot])
(defn reset-name
  "Given a robot, reset its name and return the resulting nameless robot."
  [robot]
  (reset! robot nameless-robot))
