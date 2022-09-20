;; Original code from Juxt Aero

(ns edn-config.alpha.core
  #?(:clj (:import (clojure.lang PersistentQueue))))

;; Queue utilities
(defn- queue
  [& xs]
  (into #?(:clj  PersistentQueue/EMPTY
           :cljs cljs.core/PersistentQueue.EMPTY)
        xs))

(defn- qu
  [coll]
  (apply queue coll))

(defn reassemble
  [this queue]
  ((get (meta this) `reassemble) this queue))

(defn kv-seq
  "Implementation detail.  DO NOT USE.  Will be private once out of alpha."
  [x]
  (cond
    (and (map? x) (not (record? x)))
    (with-meta
      (into [] x)
      {`reassemble (fn [_ queue] (into (empty x) queue))})

    (set? x)
    (with-meta (map-indexed (fn [idx v] [idx v]) x)
               {`reassemble (fn [_ queue]
                              (into (empty x)
                                    (map second queue)))})

    (vector? x)
    (with-meta (map-indexed (fn [idx v] [idx v]) x)
               {`reassemble (fn [_ queue]
                              (into (empty x)
                                    (mapv second (sort-by first queue))))})

    (seq? x)
    (with-meta (map-indexed (fn [idx v] [idx v]) x)
               {`reassemble (fn [_ queue]
                              (with-meta
                                (apply list (map second (sort-by first queue)))
                                (meta x)))})
    ;; Scalar value
    :else
    nil))

;; Expansion code
(defmulti eval-tagged-literal
          "Dispatches a tagged literal with control over eval.  Dispatch happens on the
          :tag of the tagged-literal. opts are the options passed to
          edn-config.core/read-config. env is a map of already resolved parts of the config.
          ks is a vector of keys which make up the current position of the tagged
          literal."
          (fn [tagged-literal opts env ks] (:tag tagged-literal)))

;; An expansion returns a map containing:
;; incomplete? - Indicating whether the evaluation completed or not
;; env - The new value of the environment bindings if appropriate
;; value - The new value for this expansion (may be a tagged-literal which needs to be requeued, or a complete value)
(declare expand)
(declare expand-coll)
(declare expand-scalar)

(defn expand-scalar
  "Expand value x without expanding any elements it may have.  Takes either a scalar or a collection (which will be treated as a scalar)."
  [x opts env ks]
  (if (tagged-literal? x)
    (eval-tagged-literal x opts env (conj ks :form))
    {:edn-config.core/value x
     :edn-config.core/env   (assoc env ks x)}))

(def ^:private ^:dynamic *max-skips* 1)

(defn expand-coll
  "Expand value x as a collection. Does not work with non-collection values."
  [x opts env ks]
  (let [steps (kv-seq x)]
    (loop [q (qu steps)
           ss []
           env env
           skip-count {}
           skipped #{}]
      (if-let [[k v :as item] (peek q)]
        (let [{; Ignore env from k expansion because values from k are not
               ; stored in env.  This decision may need to be revised in the
               ; future if funky keys such as those which can alter alternative
               ; parts of the map are wanted.

               ;env :edn-config.core/env
               k             :edn-config.core/value
               k-incomplete? :edn-config.core/incomplete?
               env           :edn-config.core/env
               :or           {env env}
               :as           k-expansion}
              (expand k opts env (conj ks k :edn-config.core/k))

              {:keys [edn-config.core/env edn-config.core/value edn-config.core/incomplete?]
               :or   {env env}
               :as   expansion}
              (when-not k-incomplete?
                (expand v opts env (conj ks k)))]
          (if (or k-incomplete? incomplete?)
            (if (<= *max-skips* (get skip-count item 0))
              (recur (pop q)
                     (conj ss [k value])
                     env
                     (update skip-count item (fnil inc 0))
                     (conj skipped (if k-incomplete?
                                     k-expansion
                                     expansion)))
              (recur (conj (pop q) [k value])
                     ss
                     env
                     (update skip-count item (fnil inc 0))
                     skipped))
            (recur (pop q)
                   (conj ss [k value])
                   (assoc env (conj ks k) value)
                   skip-count
                   skipped)))

        {:edn-config.core/value       (reassemble steps ss)
         :edn-config.core/env         env
         :edn-config.core/incomplete? (some #(>= % *max-skips*) (vals skip-count))
         :edn-config.core/incomplete  (some :edn-config.core/incomplete skipped)
         ;; Not used anywhere, but useful for debugging
         :edn-config.core/_ss         ss}))))

(defn expand
  "Expand value x.  Dispatches on whether it's a scalar or collection.  If it's
  a collection it will expand the elements of the collection."
  [x opts env ks]
  (if (or (and (map? x) (not (record? x))) (set? x) (seq? x) (vector? x))
    (expand-coll x opts env ks)
    (expand-scalar x opts env ks)))

(defn expand-scalar-repeatedly
  "Expand value x until it is either incomplete or no longer a tagged-literal.
  Use this to support chained tagged literals, e.g. #or #profile {:dev [1 2]
                                                                  :prod [2 3]}"
  [x opts env ks]
  (loop [x x]
    (let [x (expand-scalar x opts env ks)]
      (if (and (tagged-literal? (:edn-config.core/value x))
               (not (:edn-config.core/incomplete? x)))
        (recur (:edn-config.core/value x))
        x))))

(defn- expand-keys
  [m opts env ks]
  (loop [ks (keys m)
         m m]
    ;; Can't use k here as `false` and `nil` are valid ks
    (if (seq ks)
      (let [{:keys [:edn-config.core/incomplete? :edn-config.core/value] :as expansion}
            (expand (first ks) opts env ks)]
        (if incomplete?
          (assoc expansion
            :edn-config.core/value
            (-> m
                ;; Dissoc first, as k may be unchanged
                (dissoc (first ks))
                (assoc value (get m (first ks)))))
          (recur (rest ks)
                 (-> m
                     ;; Dissoc first, as k may be unchanged
                     (dissoc (first ks))
                     (assoc value (get m (first ks)))))))
      {:edn-config.core/value m})))

(defn- expand-set-keys [m]
  (reduce-kv
    (fn [m k v]
      (if (set? k)
        (reduce #(assoc %1 %2 v) m k)
        (assoc m k v))) {} m))

(defn- rewrap
  [tl]
  (fn [v]
    (tagged-literal (:tag tl) v)))

(defn expand-case
  "Expands a case-like value, in the same way as #profile, #user, etc.

  case-value is the value to dispatch on, e.g. the result of
  (System/getenv \"USER\") for #user.

  tl is the tagged-literal where the :value is a map to do the casing on.

  See implementation of #profile for an example of using this function from
  eval-tagged-literal."
  [case-value tl opts env ks]
  (let [{m-incomplete? :edn-config.core/incomplete?
         m             :edn-config.core/value
         :as           m-expansion}
        (expand-scalar-repeatedly (:form tl) opts env ks)

        {ks-incomplete? :edn-config.core/incomplete?
         :keys          [:edn-config.core/value] :as ks-expansion}
        (when-not m-incomplete?
          (expand-keys m opts env ks))]
    (if (or m-incomplete? ks-incomplete?)
      (update (or m-expansion ks-expansion) :edn-config.core/value (rewrap tl))
      (let [set-keys-expanded (expand-set-keys value)]
        (expand (get set-keys-expanded case-value
                     (get set-keys-expanded :default))
                opts env ks)))))
