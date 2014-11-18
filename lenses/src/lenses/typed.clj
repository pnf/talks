(ns lenses.typed
  (:require [clojure.core.typed :as t]
            [clojure.walk :refer [macroexpand-all]]
            [clojure.pprint :refer [pprint]])
  (:require [clojure.set :as cs :refer [union]]))

(t/ann-record Point [x :- t/Num, y :- t/Num])
(defrecord Point [x y])
(t/ann-record Color [r :- t/Int, g :- t/Int, b :- t/Int])
(defrecord Color [r g b])
(t/ann-record Turtle [position :- Point,
                      color :- Color
                      heading :- t/Num])
(defrecord  Turtle [position color heading])



(t/def myrtle :- Turtle
  (->Turtle (->Point 3.5 5.5) (->Color 0 255 0) (/ Math/PI 4.)))


(t/defn straighten [tu :- Turtle] :- Turtle
  (assoc tu :heading 0.0))                    ;!       




;!
#_(t/defn set-heading [tu :- Turtle
                     h :- String]
  (let [h (try (Double/parseDouble h) (catch Exception _ nil))]
    (assoc tu :heading h)
    ))


#_(t/cf (t/fn [x :- Double ] (if (> 0.0 x) (Math/log x) "Bleh")))


(t/defalias Heading (t/HMap :mandatory {:angle Double
                                     :units (t/U (t/Val :radians) (t/Val :degrees))}) )

#_(t/defn set-headings [tu :- Turtle
                        headings :- (t/Seq Heading)]
  (map (t/fn [h :- Heading]
         (assoc tu :heading (condp = (:units h)
                                  :radians (:angle h)
                                  :degrees (* (/ (:angle h) 180.) Math/PI))) headings)))




#_(t/defn unionize [s :- (t/Set t/Int)]
  (let [s (union s #{"hi" "there"})]
    (map inc s)))






(t/ann ^:no-check mp-union (t/All [x [x1 :< x :> x]]
                                (t/IFn [(t/Set x) (t/Set x1) * -> (t/Set x1)])))
(def mp-union union)






(t/defn set-x [tu :- Turtle
               x :- String] :- Turtle
               (assoc-in tu [:position :x] x))

