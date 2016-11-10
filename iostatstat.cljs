#!/usr/bin/env planck

(ns iostatstat.core
  (:require [clojure.string :refer [split trim]]
            [cljs.tools.reader :as r]
            [planck.core :refer [*command-line-args* slurp]]))

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn median [coll]
  (let [ns (sort coll) cnt (count ns)
        mid (bit-shift-right cnt 1)]
    (if (odd? cnt)
      (nth ns mid)
      (/ (+ (nth ns mid) (nth ns (dec mid))) 2))))

(defn split-lines
  "Splits s on \n or \r\n."
  [s]
  (split s #"\n|\r\n"))

(def not-nil? (complement nil?))

(defn read-number [s]
  (try
    (let [n (r/read-string s)]
      (if (number? n) n s))
    (catch js/Error e nil)))

(defn string-valid? [seq]
  (and (= 11 (count seq))
       (every? number? (butlast seq))))

(defn parse-line [line]
  (let [str (map read-number (split (trim line) #"\s+"))]
    (if (string-valid? str)
      str
      nil)))

(defn read-file [filename]
  (try
    (if-let [content (slurp filename)]
      content
      nil)
    (catch js/Error e
      (println "Error reading file" filename))))

(defn parse-iostat [filename]
  (if-let [content (read-file filename)]
    (let [iostat (split-lines content)]
      (filter not-nil? (map parse-line iostat)))
    nil))

(defn asvc [matrix]
  (map #(nth % 7) matrix))

(defn device [matrix re]
  (filter #(re-matches re (str (nth % 10))) matrix))

(if-let [filename (first *command-line-args*)]
  (do
    (if-let [matrix (parse-iostat filename)]
      (do
        (println "Mean asvc on disk devices" (mean (asvc (device matrix #"^c.+"))))
        (println "Median asvc on disk devices" (median (asvc (device matrix #"^c.+"))))
      (println "Error parsing file" filename)))
  (println "Usage: iostatstat <iostat-xpnC.out"))

;; Tue Nov  8 11:04:15 MSK 2016 - started
;;                     extended device statistics
;;  r/s    w/s   kr/s   kw/s wait actv wsvc_t asvc_t  %w  %b device
;; 0.0    0.0    0.0    0.0  0.0  0.0    0.0    0.2   0   0 c0t1d0s2

;;                 extended device statistics
;; r/s    w/s   kr/s   kw/s wait actv wsvc_t asvc_t  %w  %b device
;; 1.9    3.8  181.5  115.2  0.0  0.1    0.0   16.9   0   1 c0
