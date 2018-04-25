(ns history
  (:require [schema.core :as s]
            [clojure.string :as string]
            [clojure.tools.logging :as log]))

; Note: Can do this in python or any other language too!

; Browser history:
; Browser should support URL bar, forward and back buttons

; creates the history object - use a class, record, or other structure if you want!

; stores at most `max-count` URLs in the history

(defn not-negative? [n]
  (not (neg? n)))

(s/defschema NonNegInt (s/constrained s/Int not-negative? "not-negative"))
(s/defschema PosInt (s/constrained s/Int pos? "positive"))

(s/defschema History
  {:max-count PosInt
   :index (s/maybe NonNegInt)
   :entries (s/maybe [s/Str])})

(defn- valid-history? [history]
  (try
    (s/validate History history)
    (catch Exception e
      (log/error "data for schema is invalid:" (.getMessage e))
      false)))

(defn- max-index
  "Calculates max index on an array."
  [coll]
  (some-> coll not-empty count (- 1)))

(defn- empty-history? [history]
  (-> history :entries empty?))

(defn- at-start?
  "Pred to indicate whether currently at the start of a non-empty history"
  [history]
  (and (not (empty-history? history))
       (= 0 (:index history))))

(defn- at-end?
  "Pred to indicate whether currently at the start of a non-empty history"
  [history]
  (and (not (empty-history? history))
       (= (-> history :entries max-index)
          (:index history))))

(defn new-history [max-count current-index entries]
  {:post [(valid-history? %)]}
  (when (not (empty? entries))
    (assert (<= 0 current-index (max-index entries))
            (format "Invalid data: current-index %d, max index %d, entries count %s"
                    current-index (max-index entries) (count entries))))
  (assert (<= (count entries) max-count)
          (format "Count entries %d exceeded max-count %d"
                  (count entries)
                  max-count))
  {:max-count max-count
   :index current-index
   :entries entries})


(defn create-history [max-count]
  (new-history max-count nil nil))

; to support “click a link” on the browser
(defn visit [history url]
  {:pre [(valid-history? history)
         (not (string/blank? url))]}
  (let [max-count (:max-count history)
        truncated (if (empty-history? history)
                    []
                    (-> history
                        :index
                        (+ 1)
                        (take (:entries history))))
        entries (->> url
                     (conj (into [] truncated))
                     (take-last max-count))
        next-index (if (empty-history? history)
                     0
                     (min (max-index entries)
                          (+ (:index history) 1)))]
    (new-history max-count
                 next-index
                 entries)))

; to support “go back” on the browser
(defn back [history]
  (if (or (empty-history? history)
          (at-start? history))
    history
    (new-history (:max-count history)
                 (- (:index history) 1)
                 (:entries history))))

; to support; to support “go forward” on the browser
(defn fwd [history]
  (if (or (empty-history? history)
          (at-end? history))
    history
    (new-history (:max-count history)
                 (+ (:index history) 1)
                 (:entries history))))

(defn substring? [s sub]
  (not= -1 (.indexOf s sub)))

; Bonus: look up matching URLs by substring
(defn lookup [history s]
  (->> history
       :entries
       (filter #(substring? % s))
       seq))