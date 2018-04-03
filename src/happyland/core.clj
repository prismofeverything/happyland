(ns happyland.core
  (:require
   [clojure.set :as set]
   [clojure.edn :as edn]
   [clojure.string :as string]
   [clojure.pprint :as pprint]
   [google-apps-clj.google-sheets-v4 :as sheets]))

(def default-creds "resources/google-creds.edn")
(def default-sheet "1-Xk4iNdYvjBL2Kb3sEpvST4ffasnmnp-fdf0zj2A8oU")

(defn load-auth
  ([] (load-auth default-creds))
  ([path]
   (sheets/build-service (edn/read-string (slurp path)))))

(defn get-document
  ([auth] (get-document auth default-sheet))
  ([auth id]
   (sheets/get-spreadsheet-info auth id)))

(def sheet-layout
  {"Places"
   {:row 21
    :column "Q"}
   "Denizens"
   {:row 8
    :column "P"}
   "Items"
   {:row 13
    :column "N"}
   "Missions"
   {:row 2
    :column "B"}})

(defn collate-rows
  [all]
  (let [header (first all)
        rows (rest all)]
    (map
     (fn [row]
       (into {} (map vector header row)))
     rows)))

(defn sheet-map
  [auth id]
  (let [document (get-document auth id)
        sheets (get document "sheets")]
    (reduce
     (fn [m sheet]
       (let [title (get sheet "title")
             layout (get sheet-layout title)
             grid (str title "!A1:" (:column layout) (:row layout))
             data (first (sheets/get-cell-values auth id [grid]))]
         (assoc m title (collate-rows data))))
     {} (map #(get % "properties") sheets))))

(defn split-cell
  [cell]
  (let [all (mapcat
             (fn [condition]
               #(= \( (first %))
               (map
                string/trim
                (string/split condition #"&")))
             (string/split cell #","))]
    (remove #(= \( (first %)) all)))

(defn extract-matches
  [match row]
  (let [matches
        (filter
         (fn [[key value]]
           (and value (re-find match key)))
         row)]
    [(get row "name")
     (mapcat
      (fn [[key value]]
        (split-cell value))
      matches)]))

(defn merge-matches
  [match rows]
  (reduce
   (fn [m row]
     (let [[name matches] (extract-matches match row)
           inverse (into {} (map (fn [match] [match [name]]) matches))]
       (merge-with concat m inverse)))
   {} rows))

(defn merge-rows
  [sheets]
  (apply concat (vals sheets)))

(defn causal-network
  [sheets]
  (let [rows (merge-rows sheets)
        conditions (merge-matches #"condition" rows)
        effects (merge-matches #"effect" rows)
        verbs (set (concat (keys conditions) (keys effects)))]
    (into
     {}
     (map
      (fn [verb]
        [verb {:needed (get conditions verb) :provided (get effects verb)}])
      verbs))))

(defn print-network
  [network]
  (pprint/pprint (sort-by first network)))
