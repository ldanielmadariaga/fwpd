;; In ns below, notice that "gen-class" was removed
(ns fwpd.core
   ;; We haven't gone over require but we will.
  (:require [clojure.string :as s]))

(def filename "suspects.csv")

;; Later on we're going to be converting each row in the CSV into a
;; map, like {:name "Edward Cullen" :glitter-index 10}.
;; Since CSV can't store Clojure keywords, we need to associate the
;; textual header from the CSV with the correct keyword.

(def headers->keywords {"Name" :name
                        "Glitter Index" :glitter-index})

(defn str->int
  "If argument is a string, convert it to an integer"
  [str]
  (if (string? str)
    (read-string (re-find #"^-?\d+$" str))
    str))

;; CSV is all text, but we're storing numeric data. We want to convert
;; it back to actual numbers.
(def conversions {:name identity
                  :glitter-index str->int})

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(s/split % #",")
       (s/split string #"\r\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10)"
  [rows]
  (let[;; headers become the seq (:name :glitter-index
       headers (map #(get headers->keywords %) (first rows))
             ;; unmapped-rows becomes the seq
        ;; (["Edward Cullen" "10"] ["Bella Swan" "0"] ...)
        unmapped-rows (rest rows)]
    ;; Now let's return a seq of {:name "X" :glitter-index 10}
    (map (fn [unmapped-row]
            ;; We're going to use map to associate each header with its
           ;; column. Since map returns a seq, we use "into" to convert
           ;; it into a map.
           (into {}
                 ;; notice we[re passing multiple collections to map
                 (map (fn [header column]
                        ;; associate the header with the converted column
                        [header ((get conversions header) column)])
                      headers
                      unmapped-row)))
         unmapped-rows)))

(defn mapify-row
  [headers unmapped-row]
  (map (fn [header column]
       ;; associate the header with the converted column
       [header ((get conversions header) column)])
     headers
     unmapped-row))

(into {}(mapify-row [:name] ["Joe"]))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))


(glitter-filter 3 (mapify (parse (slurp filename))))

;; Gets names for the glitter filter result

(defn filtered-names
  [minimum-glitter
   records]
            (map #(get % :name ) (glitter-filter minimum-glitter records)) )

(filtered-names 3 (mapify (parse (slurp filename))))


;; Prepends suspect to the beggining of the suspect list

(defn prepend-suspect
  [suspect records]
  (into [suspect] records))

(prepend-suspect {:name "Joe Mills" :glitter-index "9001"} (mapify (parse (slurp filename))))
;; Validate that :name and :glitter-index are present when you prepend
(defn validate
  [keywords suspect]
  (and (not-empty (get suspect :name)) (not-empty (get suspect :glitter-index))))
(def bla {:id "3" :weight "100kg"})
(def joe {:name "Joe Mills" :glitter-index "9001"})
(validate {:name :glitter-index} joe)
(validate {:name :glitter-index} bla)
(get bla :name)
(get bla :id)
(nil? (get bla :name))
(nil? (get bla :id))
(and (not-empty (get bla :name)) (not-empty (get bla :glitter-index)))
(and (not-empty (get joe :name)) (not-empty(get joe :glitter-index)))

(defn prepend-suspect-with-validate
  [suspect keywords records]
  (if (nil? (validate keywords suspect))
    (records)
    (into [suspect] records)))

(prepend-suspect-with-validate joe {:id :glitter-index}  (mapify (parse (slurp filename))))
;;(prepend-suspect-with-validate {:id "3" :weight "100kg"} {:name :glitter-index}  (mapify (parse (slurp filename))))

;; Converts the list of maps back into a CSV string, uses clojure.string/join
