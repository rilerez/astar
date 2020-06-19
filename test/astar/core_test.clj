(ns astar.core-test
  (:require [clojure.test :refer :all]
            [astar.core :refer :all]))

(defrecord Graph [nodes weight neighbors])

(extend-type Graph
  WEIGHTED_GRAPH
  (nodes [{nodes :nodes}] nodes)
  (neighbors [{neighbors :neighbors} node] (neighbors node))
  (weight [{weight :weight} from to] (weight from to)))

(def edges
  {#{:a :b} 1
   #{:a :c} 10
   #{:b :c} 9
   #{:b :d} 11
   #{:d :c} 1})

(defn- edges->weight [edges]
  (fn [x y]
    (if (= x y) 0
        (get edges #{x y} ##Inf))))

(defn- edges->neighbors [edges]
  (apply merge-with (comp set concat)
         (for [[a b] (map seq (keys edges))]
           {a #{b} b #{a}})))

(defn- edges->nodes [edges]
  (->> edges edges->neighbors keys (into #{})))

(defn edges->graph
  "This function turns a map of edges (map from sets -> length) into a graph record"
  [edges]
  (Graph. (edges->nodes edges) (edges->weight edges) (edges->neighbors edges)))
