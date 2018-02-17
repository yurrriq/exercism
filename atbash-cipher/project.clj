(let [repo "https://github.com/yurrriq/exercism"
      proj "clojure/atbash-cipher"]
  (defproject atbash-cipher "0.1.0"
    :description  "atbash-cipher exercise."
    :url          ~(str repo "/tree/" proj)
    :dependencies [[org.clojure/clojure    "1.8.0"]
                   [org.clojure/core.typed "0.3.22"]
                   [funcool/cats "1.2.1"]]
    :plugins      [[lein-codox "0.9.4"]]
    :codox        {:metadata    {:doc/format :markdown}
                   :project     {:package nil}
                   :source-uri  ~(str repo "/blob/" proj "/{filepath}#L{line}")
                   :output-path ~(str "../../_site/" proj "/doc")}))
