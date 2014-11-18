(ns lenses.typed
  (:require [clojure.core.typed :as t]
            [clojure.walk :refer [macroexpand-all]]
            [clojure.pprint :refer [pprint]]
            [clojure.data.codec.base64 :as b64])
  (:require [clojure.set :as cs :refer [union]]))


(defn s->b64 [s] (String. (b64/encode (.getBytes s))))
(defn b64->s [s] (String. (b64/decode (.getBytes s))))

(def path-dict
  {:zone    [:launch-specification :placement :availability-zone]
   :public? [:launch-specification :network-interfaces 0 :associate-public-ip-address]
   :udata   [:launch-specification :user-data [s->b64 b64->s]]} )


(defn condition-key
  ([k]
     (if (sequential? k) k [k]))
  ([path-dict k]
     (cond
      (sequential? k) k
      (path-dict k)       (path-dict k)
      :else           [k])))


