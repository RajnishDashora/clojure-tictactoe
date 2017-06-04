(ns tictactoe.core
  (:gen-class))

(def empty-board [[" " " " " "] [" " " " " "] [" " " " " "]])

(defn has-empty-square?
  [board]
  (some #(= " " %) (flatten board)))

(def opponent {"X" "O" "O" "X"})

(defn transpose-board
  [board]
  (apply mapv vector board))

(defn show-row
  [row]
  (str "| " (clojure.string/join " | " row) " |"))

(defn show-board
  [board]
  (clojure.string/join "\n" (for [row board] (show-row row))))

(defn get-square
  [board x y]
  {:pre [(<= 0 x 2) (<= 0 y 2)]}
  (get-in board [x y]))

(defn check-rows
  [board v]
  (some true?
        (for [i (range 3)] (apply = v (get board i)))))

(defn check-cols
  [board v]
  (some true?
        (for [i (range 3)]
          (apply = v
                 (for [j (range 3)] (get-in board [j i]))))))

(defn check-diagonals
  [board v]
  (or
   (apply = v (for [i (range 3)] (get-in board [i i])))
   (apply = v (for [i (range 3)] (get-in board [i (- 2 i)])))))

(defn winner?
  [board v]
  (some true? ((juxt check-rows check-cols check-diagonals) board v)))

(defn draw?
  [board]
  (not
   (or (has-empty-square? board)
       (winner? board "X")
       (winner? board "O"))))

(defn mark-square
  [board x y v]
  {:pre [(<= 0 x 2)
         (<= 0 y 2)
         (or (= v "X") (= v "O") (= v " "))]}
  (when (= (get-square board x y) " ")
    (assoc-in board [x y] v)))

(defn game-over?
  [board]
  (or (winner? board "X")
      (winner? board "O")
      (not (has-empty-square? board))))

(defn number-of-empty-squares
  [board]
  (apply + (for [i (range 3)] ((frequencies (get board i)) " " 0))))

(defn whose-move?
  [board]
  (if (odd? (number-of-empty-squares board))
    "X"
    "O"))

(defn next-boards
  [board v]
  (when-not (game-over? board)
    (remove nil?
            (for [i (range 3) j (range 3)]
              (mark-square board i j v)))))

(defn possible-moves [board v]
  (filter #(not (winner? % (opponent v))) (next-boards board v)))

(defn evaluate [board v]
  (cond
    (winner? board v) (* 10 (number-of-empty-squares board))
    (winner? board (opponent v)) (* -10 (number-of-empty-squares board))
    (draw? board) 0
    :else nil))

(defn find-min-score [board v]
  (if-let [score (evaluate board v)]
    score
    (let [child-boards (next-boards board (opponent v))
          possible-scores (map #(* -1 (find-min-score % (opponent v))) child-boards)]
      (apply min possible-scores))))


;; minimize for opponent when you can play
(defn possible-scores
  [board v]
  (for [i (range 3) j (range 3)]
    (if (mark-square board i j v)
      (find-min-score (mark-square board i j v) v)
      nil)))

(defn maximize [scores]
  (apply max (remove nil? scores)))

(defn best-move
  [board v]
  (let [all-possible-scores (possible-scores board v)
        max-score (maximize all-possible-scores)
        index (.indexOf all-possible-scores max-score)]
    (cond
      (= index 0) {:x 0 :y 0}
      (= index 1) {:x 0 :y 1}
      (= index 2) {:x 0 :y 2}
      (= index 3) {:x 1 :y 0}
      (= index 4) {:x 1 :y 1}
      (= index 5) {:x 1 :y 2}
      (= index 6) {:x 2 :y 0}
      (= index 7) {:x 2 :y 1}
      (= index 8) {:x 2 :y 2})))


(defn move
  [board v]
  (let [{:keys [x y]} (best-move board v)]
    (if-let [next-board (mark-square board x y v)]
      next-board
      (recur board v))))

(defn gen-random-board
  ([]
   (gen-random-board empty-board "X"))
  ([board v]
   (if-not (has-empty-square? board)
     board
     (let [x (rand-int 3)
           y (rand-int 3)]
       (if-let [next-board (mark-square board x y v)]
         (recur next-board (opponent v))
         (recur board v))))))

(defn game
  ([]
   (game empty-board "X"))
  ([board v]
   (println (show-board board))
   (println " ")
   (if-not (game-over? board)
     (recur (move board v) (opponent v))
     [(winner? board "X") (winner? board "O") (draw? board)])))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
