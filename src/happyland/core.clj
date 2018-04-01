(ns happyland.core
  (:require
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
   {:row 11
    :column "Q"}
   "Denizens"
   {:row 8
    :column "P"}
   "Items"
   {:row 13
    :column "N"}})

(defn sheet-map
  [auth id]
  (let [document (get-document auth id)
        sheets (get document "sheets")]
    (reduce
     (fn [m sheet]
       (let [title (get sheet "title")
             layout (get sheet-layout title)
             grid (str title "!A1:" (:column layout) (:row layout))
             data (sheets/get-cell-values auth id [grid])]
         (assoc m title data)))
     {} (map #(get % "properties") sheets))))

