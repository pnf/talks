(ns lenses.holes
  (:require [clojure.core.typed :as t]
            [schema.core :as s  :include-macros true]
            [clojure.walk :refer [macroexpand-all]]
            [clojure.pprint :refer [pprint]]
            [clojure.data.codec.base64 :as b64]
            [clojure.data.json :as json]            )

  (:require [clojure.set :as cs :refer [union]]))



(t/ann ^:no-check s->b64 [String -> String])
(defn s->b64 [s] (String. (b64/encode (.getBytes s))))
(t/ann ^:no-check b64->s [String -> String])
(defn b64->s [s] (String. (b64/decode (.getBytes s))))


(def path-dict
  {:zone    [:launch-specification :placement :availability-zone]
   :public? [:launch-specification :network-interfaces 0 :associate-public-ip-address]
   :udata   [:launch-specification :user-data [s->b64 b64->s]]} )


  (t/defalias NetworkInterface
    (t/HMap :mandatory {:device-index t/Int
                        :subnet-id String
                        :groups (t/Vec String)}))
  (t/defalias Launch-Specification
    (t/HMap :mandatory {:image-id String,
                        :instance-type String,
                        :placement  (t/HMap :mandatory {:availability-zone String}),
                        :key-name String,
                        :user-data String,
                        :network-interfaces (t/Vec NetworkInterface)
                        :iam-instance-profile (t/HMap :mandatory {:arn String})}))
(t/defalias Spot-Request
  (t/HMap :mandatory {:spot-price Double
                     :type String
                     :launch-specification Launch-Specification}))


(t/ann my-req Spot-Request)
(def my-req   {:spot-price 0.01, 
   :instance-count 1, 
   :type "one-time", 
   :launch-specification
   {:image-id "ami-something",
    :instance-type "t1.micro",
    :placement  {:availability-zone "us-east-1a"},
    :key-name "your-key"
    :user-data "WWFua2VlIGRvb2RsZSB3ZW50IHRvIHRvd24gcmlkaW5nIG9uIGEgcG9ueQo="
    :network-interfaces
    [{:device-index 0
      :subnet-id "subnet-yowsa"
      :groups ["sg-hubba"]}]
    :iam-instance-profile
    {:arn "arn:aws:iam::123456789:instance-profile/name-you-chose"}}}  )

#_(t/ann ^:no-check SpotRequestSchema (t/HMap :complete? false))
#_(def SpotRequestSchema
  {:spot-price s/Num
   :instance-count s/Int
   :type s/Str
   :launch-specification
   {:image-id s/Str
    :instance-type s/Str
    :placement {:availability-zone s/Str}
    :key-name s/Str
    :user-data s/Str
    :network-interfaces [{:device-index s/Int :subnet-id s/Str :groups [s/Str]}]
    :iam-instance-profile {:arn s/Str}}})

#_(s/validate SpotRequestSchema my-req)


(t/ann condition-key
    [(t/Map t/Keyword (t/Seq (t/U t/Int t/Keyword [t/Any -> t/Any])))
     (t/U t/Int t/Keyword)
     -> (t/Option (t/Seq (t/U t/Int t/Keyword [t/Any -> t/Any])))])
(defn condition-key
  [path-dict k] (if (keyword? k)(path-dict k) [k]))


#_(t/cf (get (t/ann-form {:a 3} (t/HMap :mandatory {:a t/Int})) :a ))


(defmacro th-get-in1 [m path]
  (reduce (fn [acc k] (concat acc `((get ~k))))
          `(-> ~m) path))

(defmacro th-get-in [m path]
  (reduce (fn [acc k] (concat acc (list (if (vector? k)
                                          `(~(second k))
                                          `(get ~k)))))
          `(-> ~m) path))



