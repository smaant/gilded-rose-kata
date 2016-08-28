(ns gilded-rose.core)

(def ^:const min-quality 0)
(def ^:const max-quality 50)

(defn- dec-min
  "Decrease given `val` by `dec-by` (or 1), but not lower than `min-val`"
  ([val min-val]        (dec-min val 1 min-val))
  ([val dec-by min-val] (max (- val dec-by) min-val)))

(defn- inc-max
  "Increase given `val` but not higher than `max-val`"
  [val max-val]
  (min (inc val) max-val))

(defmulti update-item :name)

(defmethod update-item "+5 Dexterity Vest"
  [item]
  (-> item
      (update :sell-in dec)
      (as-> {:keys [sell-in] :as item*}
        (cond-> item*
          (neg? sell-in) (update :quality dec-min min-quality)
          :always        (update :quality dec-min min-quality)))))

(defmethod update-item "Elixir of the Mongoose"
  [item]
  (-> item
      (update :sell-in dec)
      (as-> {:keys [sell-in] :as item*}
        (cond-> item*
          (neg? sell-in) (update :quality dec-min min-quality)
          :always        (update :quality dec-min min-quality)))))

(defmethod update-item "Aged Brie"
  [item]
  (-> item
      (update :sell-in dec)
      (update :quality inc-max max-quality)))

(defmethod update-item "Backstage passes to a TAFKAL80ETC concert"
  [item]
  (-> item
      (update :sell-in dec)
      (as-> {:keys [sell-in] :as item*}
        (cond-> item*
          (>= sell-in 0) (update :quality inc-max max-quality)
          (< sell-in 10) (update :quality inc-max max-quality)
          (< sell-in 5)  (update :quality inc-max max-quality)
          (neg? sell-in) (assoc :quality 0)))))

(defmethod update-item "Sulfuras, Hand Of Ragnaros"
  [item]
  item)

(defmethod update-item "Conjured Belt"
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
