(ns lens-talk
  (:require [schema.core :as s 
             :include-macros true]
   [clojure.core.typed :as t]
            [clojure.walk :refer [macroexpand-all]]
            [clojure.pprint :refer [pprint]]))

(t/ann-record Point [x :- Number, y :- Number])
(t/ann-record Color [r :- Short, g :- Short, b :- Short])
(t/ann-record Turtle [position :- Point,
                      color :- Color
                      heading :- Number])
