(let [repo "https://github.com/yurrriq/exercism"
      proj "clojure/bob"]
  (defproject bob "0.4.0"
    :description  "bob exercise."
    :url          "https://yurrriq.github.io/exercism/clojure/bob"
    :dependencies [[org.clojure/clojure    "1.8.0"]
                   [org.clojure/core.typed "0.3.22"]]

    :plugins      [[lein-codox "0.9.4"]]
    :codox        {:metadata    {:doc/format :markdown}
                   :project     {:package nil}
                   :source-uri  ~(str repo "/blob/" proj "/{filepath}#L{line}")
                   :output-path "../../_site/clojure/bob/doc"}))
