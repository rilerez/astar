(ns astar.core
  (:use clojure.data.priority-map))

(defprotocol WEIGHTED_GRAPH
  (nodes [graph])
  (neighbors [graph node])
  (weight [graph from to]))

(defn- calc-approx-dist [h dist]
  (for [[node d] dist]
    [node (+ d (h node))]))

;;; when you visit a node, you found the shortest path to that node already
;;; follows from the triangle inequality
(defn astar
  "Finds a path from `start` to `goal` in `graph` (a `WEIGHTED_GRAPH`),using
  `heuristic` to approximate the distance. Return `nil` if no path is found. If
  `goal` is `nil`, just search until the connected component of the graph
  containing `start` is fully explored, returning a map nodes -> paths."
  [start goal graph heuristic]
  (let [guess-goal-dist #(heuristic % goal)
        nodes (nodes graph)
        neighbors #(neighbors graph %)
        weight #(weight graph %1 %2)]
    (loop [;; A map nodes -> known-distance
           known-dist (merge (into {} (for [x nodes] [x ##Inf]))
                             {start 0})
           ;; A priority-map of nodes -> known-dist+heuristic. Sorted min known-distance first.
           guess-unseen-dist (into (priority-map)
                                   (calc-approx-known-dist guess-goal-dist known-dist))
           visited? #{}
           ;; mapping nodes -> paths
           path {start []}]
      (let [[best-unseen _] (peek guess-unseen-dist)]
        (if (or (= (known-dist best-unseen) ##Inf) ; no path from start to goal
                (visited? goal)
                (empty? guess-unseen-dist))
          (if goal (path goal) path)
          (let [;; [neighbor known-distance] when they're closer than we thought
                closer-nbrs (for [nbr (neighbors best-unseen)
                                  :let [new-known-dist (+ (known-dist best-unseen)
                                                          (weight best-unseen nbr))]
                                  :when (< new-known-dist (known-dist nbr))]
                              [nbr new-known-dist])
                ;; the paths through the current node to
                closer-paths (for [[nbr _] closer-nbrs]
                               [nbr (conj (path best-unseen) nbr)])]
            (recur (into known-dist closer-nbrs)
                   (into (pop guess-unseen-dist)
                         (calc-approx-dist guess-goal-dist closer-nbrs))
                   (conj visited? best-unseen)
                   (into path closer-paths))))))))

(defn dijkstra
  "Performs a search. From `start` to `goal` in `graph`. If `goal` is `nil`,
  find the map nodes -> shortest path from `start`."
  ([start goal graph] (astar start goal graph (fn [& _] 0)))
  ([start graph] (dijkstra start nil graph)))

(def costs
  [[1  1  1  1]
   [1  1  ##Inf  ##Inf]
   [1  1  ##Inf 1]])

(defn dims [array]
  [(count (first array)) (count array)])
(defn lookup [array [x y]]
  (get-in array [y x]))

(defn cross [lst1 lst2] (for [x lst1 y lst2] [x y]))
(def nodes% (set (apply cross (map range (dims costs)))))

(defn in-bounds? [[x y]]
  (let [[dx dy] (dims costs)]
    (and (<= 0 x (dec dx))
         (<= 0 y (dec dy)))))

(defn neighbors% [node]
  (filter in-bounds?
          (for [delta (concat (cross [0] (range -1 2))
                              (cross (range -1 2) [0]))
                :when (not= delta [0 0])]
            (vec (map + node delta)))))

(defn euclidean [p1 p2]
  (Math/sqrt
   (reduce + (map #(Math/pow % 2)
                  (map - p1 p2)))))

(defn weight% [p1 p2]
  {:pre [(every? in-bounds? [p1 p2])]}
  (* (euclidean p1 p2)
     (lookup costs p2)))

(defn make-graph [& {neighbors :neighbors
                     nodes :nodes
                     weight :weight}]
  (reify WEIGHTED_GRAPH
    (nodes [_] nodes)
    (neighbors [_ node] (neighbors node))
    (weight [_ x y] (weight x y))))

(def board (repeat 10 (repeat 10 (ref nil))))
(astar [0 0] [1 2] (make-graph :neighbors neighbors% :weight weight% :nodes nodes%)
       euclidean)
