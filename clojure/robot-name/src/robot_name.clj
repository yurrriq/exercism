(ns robot-name
  "Managing robot factory settings."
  {:author "Eric Bailey"}
  (:require [clojure.pprint :refer [cl-format]])
  (:import (java.util Random)))

;;;; == PRIVATE API ============================================================

(def ^:private nameless-robot
  "A map representing a robot with the name, \"\"."
  {:name ""})


(def ^:private random
  "A random number generator."
  (Random.))


(def ^:private capital-letters
  "A sequence of the capital letters, A-Z."
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ")


(defn- rand-capital-letter
  "Return a random capital letter."
  []
  (rand-nth capital-letters))


(defn- rand-number
  "Return a random integer from 0 through 999."
  ([]          (rand-number random))
  ([^Random r] (. r (nextInt 1000))))


(defn- rand-name
  "Return a random robot name of the form \"AA000\"."
  []
  (cl-format nil "~A~A~3,'0d"
             (rand-capital-letter)
             (rand-capital-letter)
             (rand-number)))


;;;; == PUBLIC API =============================================================

(defn robot
  "Create and return a new, nameless robot."
  []
  (atom nameless-robot))


(defn robot-name
  "Given a robot, if it has a name, return it,
  otherwise assign it a new name and return that."
  [robot]
  (or (not-empty (:name @robot))
      (:name (swap! robot assoc :name (rand-name)))))


(defn reset-name
  "Given a robot, reset its name and return the resulting nameless robot."
  [robot]
  (reset! robot nameless-robot))
