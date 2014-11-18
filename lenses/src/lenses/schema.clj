(ns lenses.schema
  (:require [schema.core :as s 
             :include-macros true]))


(s/defrecord Point [x :- s/Num, y :- s/Num])
(s/defrecord Color [r :- s/Int g :- s/Int b :- s/Int])
(s/defrecord  Turtle [position :- Point color :- Color heading :- s/Num])


(def myrtle (->Turtle (->Point 3.5 2.5) (->Color 0 255 0) 2.1))
(def bad-myrtle (->Turtle (->Point 3.5 "2.5") (->Color 0 255 0) 2.1))

(s/defn set-heading [tu :- Turtle x :- s/Str] :- Turtle
  (assoc tu :heading x)  )

(s/with-fn-validation (set-heading myrtle "thisway"))



(s/defn set-x [tu :- Turtle x :- s/Str] :- Turtle
  (assoc-in tu [:position :x] x))

