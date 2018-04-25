(ns history
  (:require [schema.core :as s]
            [clojure.string :as string]
            [clojure.tools.logging :as log]))

; Note: Can do this in python or any other language too!

; Browser history:
; Browser should support URL bar, forward and back buttons

; creates the history object - use a class, record, or other structure if you want!

; stores at most `max` URLs in the history

(defn not-negative? [n]
  (not (neg? n)))

(s/defschema NonNegInt (s/constrained s/Int not-negative? "not-negative"))
(s/defschema PosInt (s/constrained s/Int pos? "positive"))

(s/defschema History
  {:max PosInt
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
  (some-> coll not-empty count dec))

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

(defn new-history [max current-index entries]
  {:post [(valid-history? %)]}
  (when (not (empty? entries))
    (assert (<= 0 current-index (max-index entries))
            (format "Invalid data: current idx %d, max idx %d, entries count %s"
                    current-index (max-index entries) (count entries))))
  (assert (<= (count entries) max)
          (format "Count entries %d exceeded max %d"
                  (count entries)
                  max))
  {:max max
   :index current-index
   :entries entries})


(defn create-history [max]
  (new-history max nil nil))

; to support “click a link” on the browser
(defn visit [history url]
  {:pre [(valid-history? history)
         (not (string/blank? url))]}
  (let [max (:max history)
        truncated (when-not (empty-history? history)
                    (-> history :index inc (take (:entries history))))
        entries (->> url (conj (into [] truncated)) (take-last max))
        next-index (if (empty-history? history)
                     0
                     (min (max-index entries) (inc (:index history))))]
    (new-history max next-index entries)))

; to support “go back” on the browser
(defn back [history]
  {:pre [(valid-history? history)]}
  (if (or (empty-history? history)
          (at-start? history))
    history
    (new-history (:max history) (dec (:index history)) (:entries history))))

; to support; to support “go forward” on the browser
(defn fwd [history]
  {:pre [(valid-history? history)]}
  (if (or (empty-history? history)
          (at-end? history))
    history
    (new-history (:max history) (inc (:index history)) (:entries history))))

(defn substring? [s sub]
  (not= -1 (.indexOf s sub)))

; Bonus: look up matching URLs by substring
(defn lookup [history s]
  {:pre [(valid-history? history)
         (not (string/blank? s))]}
  (->> history :entries (filter #(substring? % s)) seq))