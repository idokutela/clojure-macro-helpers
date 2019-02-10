(ns macro-helpers.core
  "A set of helper functions to build clojure macros.")

(defn illegal-argument
  "Creates an IllegalArgumentException or js/Error."
  [message]
  #?(:clj (IllegalArgumentException. message)
     :cljs (js/Error message)))


(defn process-first-on-pred
  "Processes the first in `coll` with `f` if `pred`, otherwise `default`.

   Returns a pair [processed rest] if pred succeeds, or [default coll]."
  [coll f pred & {:keys [default] :or {default nil}}]
  (if (pred (first coll))
    [(f (first coll)) (rest coll)]
    [default coll]))

(defn parse-fn
  "Parses the body of a fn-expression.

  Returns a map with keys `:name` and `:fdefs`, where `:fdefs` is a
  collection of maps with keys `:params`, `:prepost` and `:body`."
  [fdecl]
  (let [[name fdecl] (process-first-on-pred fdecl identity symbol?)
        fdecl (cond (vector? (first fdecl)) (list fdecl)
                    (seq? fdecl) fdecl
                    :else (throw (illegal-argument "Parameters missing")))
        parse-body (fn [[params & body]]
                     (when-not (vector? params)
                       (throw (illegal-argument
                               (if (seq? (first fdecl))
                                 (str "Parameter declaration " params
                                      " should be a vector.")
                                 (str "Invalid signature: " fdecl
                                      " should be a list.")))))
                     (let [[prepost body]
                           (process-first-on-pred body identity map?)]
                       {:params params
                        :prepost prepost
                        :body body}))
        fdefs (map parse-body fdecl)]
    {:name name :fdefs fdefs}))


(defn build-body
  "Builds a single fn-body."
  [{:keys [params prepost body]}]
  (if (some? prepost)
    `(~params ~prepost ~@body)
    `(~params ~@body)))


(defn build-fn
  "Builds a fn-expression from a parsed fn-expression map.

  See `parse-fn` for the format of a parsed fn."
  [{:keys [name fdefs]}]
  (let [bodies (map build-body fdefs)
        bodies (if (= 1 (count bodies))
                 (first bodies)
                 bodies)]
    (if (some? name)
      `(fn ~name ~@bodies)
      `(fn ~@bodies))))


(defn parse-defn
  "Parses the body of defn-like expressions

  Returns a map with keys `:name`, `:meta`, `:fdefs`, with metadata
  the metadata map, and body a collection of maps with keys `:params`,
  `:prepost`, `:body`.

  Examples:

      (parse-defn '(f [x] (inc x)))

        => {:name f
            :metadata {}
            :fexprs '({:params [x] :prepost nil :body ((inc x))})}


      (parse-defn '(always-fails
                    \"The precondition always fails\"
                    []
                    {:pre (constantly false)}))

        => {:name always-fails
            :meta {:doc \"The precondition always fails\"}
            :fexprs '({:params []
                       :prepost {:pre (constantly false)}
                       :body nil})}

      (parse-defn '(multi-body
                    {:private true}
                    ([] 0)
                    ([_] 1)
                    ([_ _] {:post (constantly true)} 2)))

        => {:name parse-defn
            :meta {:private true}
            :fexprs '({:params [] :prepost nil :body (0)}
                      {:params [_] :prepost nil :body (1)}
                      {:params [_ _]
                       :prepost {:post (constantly true)}
                       :body (2)}))"
  [[f & fdecl]]
  (when-not (symbol? f)
    (throw (illegal-argument "The first argument must be a symbol.")))
  (let
      [[m fdecl] (process-first-on-pred
                  fdecl #(assoc {} :doc %) string? :default {})
       [m fdecl] (process-first-on-pred
                  fdecl #(conj % m) map? :default m)
       {:keys [fdefs]}  (parse-fn fdecl)]
    {:name f
     :meta m
     :fdefs fdefs}))

(defn build-defn
  "Builds a defn from a parsed defn."
  [{:keys [name meta fdefs]}]
  (let [bodies (map build-body fdefs)
        bodies (if (= 1 (count bodies))
                 (first bodies)
                 bodies)]
    (if (empty? meta)
      `(defn ~name ~@bodies)
      `(defn ~name ~meta ~@bodies))))
