(ns entropy.core
  (:gen-class)
  (:use clojure.java.io))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn slurp-bytes
  "Slurp the bytes from a slurpable thing"
  [filename]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (copy (input-stream filename) out)
    (.toByteArray out)))

(defn char-occurence
  "frequency of an element in collection"
  [c cs]
  (get (frequencies cs) c 0))

(defn char-probability
  "calculates the probability of a character inside of an information source"
  [x xs]
  (/ (char-occurence x xs) (double (count xs))))

(defn char-information
  "calculates the information content of a character"
  [c probability]
  (log2 (/ 1.0 probability)))

(defn entropy
  "Calculates the entropy of a file"
  [distinct-chars all-chars]
  (reduce + (map #(let [probability (char-probability % all-chars)
                        information (char-information % probability)]
                    (* probability information)) distinct-chars)))

(defn -main [& args]
  (if-not (empty? args)
    (let [filename (first args)
          file-chars (map int (slurp-bytes filename))
          distinct-chars (sort (distinct file-chars))]
      (printf "Character types in file: %d\n" (count distinct-chars))
      (printf "Number of character in file: %d\n" (count file-chars))
      (printf "Entropy of file: %.16f\n" (entropy distinct-chars file-chars))
      (doseq [x distinct-chars]
        (let [probability (char-probability x distinct-chars)
              information (char-information x probability)]
          (println (char-occurence x file-chars))
          (if-not (Character/isWhitespace x)
            (printf "%5c : o=%8d  p=%1.10f  i=%-2.10f%n" x (char-occurence x file-chars) probability information)
            (printf " (%d) : o=%8d  p=%1.10f  i=%-2.10f%n" x (char-occurence x file-chars) probability information)))))
    (println "Please supply a filename to analyze")))
