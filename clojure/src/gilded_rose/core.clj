(ns gilded-rose.core)

(def ^:const min-quality 0)
(def ^:const max-quality 50)

;; Have a polymorphism based on items names is not a very scalable solution.
;; Better solution is to have types of items that behave in a similar way.
;; Assuming we cannon change or extend item (by the kata definition),
;; external mapping is the only solution for adding types. Not ideal
;; one though, because requires updating this hash-map for each new name.
(def ^:const items-types {"Conjured Belt"                             :conjured
                          "Backstage passes to a TAFKAL80ETC concert" :ticket
                          "Sulfuras, Hand Of Ragnaros"                :legendary
                          "Elixir of the Mongoose"                    :normal
                          "Aged Brie"                                 :improving
                          "+5 Dexterity Vest"                         :normal})

(defn- dec-min
  "Decrease given `val` by `dec-by` (or 1), but not lower than `min-val`"
  ([val min-val]        (dec-min val 1 min-val))
  ([val dec-by min-val] (max (- val dec-by) min-val)))

(defn- inc-max
  "Increase given `val` but not higher than `max-val`"
  [val max-val]
  (min (inc val) max-val))

(defmulti update-item (comp items-types :name))

(defmethod update-item :normal
  [item]
  (-> item
      (update :sell-in dec)
      (as-> {:keys [sell-in] :as item*}
        (cond-> item*
          (neg? sell-in) (update :quality dec-min min-quality)
          :always        (update :quality dec-min min-quality)))))

(defmethod update-item :improving
  [item]
  (-> item
      (update :sell-in dec)
      (update :quality inc-max max-quality)))

(defmethod update-item :ticket
  [item]
  (-> item
      (update :sell-in dec)
      (as-> {:keys [sell-in] :as item*}
        (cond-> item*
          (>= sell-in 0) (update :quality inc-max max-quality)
          (< sell-in 10) (update :quality inc-max max-quality)
          (< sell-in 5)  (update :quality inc-max max-quality)
          (neg? sell-in) (assoc :quality 0)))))

(defmethod update-item :legendary
  [item]
  item)

(defmethod update-item :conjured
  [item]
  (-> item
      (update :sell-in dec)
      (as-> {:keys [sell-in] :as item*}
        (cond-> item*
          (neg? sell-in) (update :quality dec-min 2 min-quality)
          :always        (update :quality dec-min 2 min-quality)))))

(defn update-quality
  [items]
  (map update-item items))

(defn item
  [item-name, sell-in, quality]
  {:name item-name, :sell-in sell-in, :quality quality})

(defn update-current-inventory
  []
  (let [inventory [(item "+5 Dexterity Vest" 10 20)
                   (item "Aged Brie" 2 0)
                   (item "Elixir of the Mongoose" 5 7)
                   (item "Sulfuras, Hand Of Ragnaros" 0 80)
                   (item "Backstage passes to a TAFKAL80ETC concert" 15 20)
                   (item "Conjured Belt" 13 34)]]
    (update-quality inventory)))
