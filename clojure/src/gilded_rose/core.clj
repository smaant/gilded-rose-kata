(ns gilded-rose.core)

(def ^:const min-quality 0)
(def ^:const max-quality 50)

(defn- dec-min
  [val min-val]
  (max (dec val) min-val))

(defn- inc-max
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
                   (item "Backstage passes to a TAFKAL80ETC concert" 15 20)]]
    (update-quality inventory)))
