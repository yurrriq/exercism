(let [paths (-> (:out (clojure.java.shell/sh "ls"))
                (clojure.string/split #"\n")
                (->> (remove #{"README.org" "project.clj" "target"
                               ;; Removed until solved
                               "bank-account"
                               "prime-factors"})))]
  (defproject xclojure "0.1.0"
    :description  "Exercism Exercises in Clojure"
    :url          "https://github.com/yurrriq/exercism/tree/clojure"
    :source-paths ~(mapv #(str % "/src") paths)
    :test-paths   ~(mapv #(str % "/test") paths)
    :dependencies [[org.clojure/clojure    "1.7.0"]
                   [funcool/cats           "1.0.0"]
                   [org.clojure/core.typed "0.3.11"]]))
