(ns gilded-rose.core-spec
(:require [speclj.core :refer :all]
          [gilded-rose.core :refer [update-quality item]]))

(def vest     "+5 Dexterity Vest")
(def elixir   "Elixir of the Mongoose")
(def brie     "Aged Brie")
(def passes   "Backstage passes to a TAFKAL80ETC concert")
(def sulfuras "Sulfuras, Hand Of Ragnaros")
(def conjured "Conjured Belt")

(def ageing-items [vest elixir])

(defn items
  [names sell-in quility]
  (map #(item % sell-in quility) names))

(describe "gilded rose"
  (it "decreases sell-in of all items but Sulfuras"
    (let [names [vest elixir brie passes]]
      (doseq [item (update-quality (items names 5 5))]
        (should= 4 (:sell-in item)))))

  (it "doesn't decrease sell-in of the Sulfuras"
    (doseq [item (update-quality (items [sulfuras] 5 5))]
      (should= 5 (:sell-in item))))

  (it "doesn't change quality of the Sulfuras"
    (doseq [item (update-quality (items [sulfuras] 5 5))]
      (should= 5 (:quality item))))

  (it "decreases quality of Vest and Elixir by one when sell-in > 0"
    (doseq [item (update-quality (items ageing-items 5 5))]
      (should= 4 (:quality item))))

  (it "decreases quality of Vest and Elixir by one when sell-in = 0"
    (doseq [item (update-quality (items ageing-items 1 5))]
      (should= 4 (:quality item))))

  (it "decreases quality of Vest and Elixir by two when sell-in < 0"
    (doseq [item (update-quality (items ageing-items 0 5))]
      (should= 3 (:quality item))))

  (it "decreases quality of Conjured item by two when sell-in > 0"
    (doseq [item (update-quality (items [conjured] 5 5))]
      (should= 3 (:quality item))))

  (it "decreases quality of Conjured item by two when sell-in = 0"
    (doseq [item (update-quality (items [conjured] 1 5))]
      (should= 3 (:quality item))))

  (it "decreases quality of Conjured item by four when sell-in < 0"
    (doseq [item (update-quality (items [conjured] 0 5))]
      (should= 1 (:quality item))))

  (it "doesn't decrease quality below zero"
    (doseq [item (update-quality (items (conj ageing-items conjured) 5 0))]
      (should= 0 (:quality item))))

  (it "increases the quality of Brie every update"
    (doseq [item (update-quality (items [brie] 5 5))]
      (should= 6 (:quality item))))

  (it "increases the quality of Passes by one if more than 10 days left"
    (doseq [item (update-quality (items [passes] 11 5))] ; will be 10 after sell-in dec
      (should= 6 (:quality item))))

  (it "increases the quality of Passes by two if < 10 and  >= 5 days left"
    (doseq [item (update-quality (items [passes] 10 5))] ; will be 9 after sell-in dec
      (should= 7 (:quality item)))
    (doseq [item (update-quality (items [passes] 6 5))]  ; will be 5 after sell-in dec
      (should= 7 (:quality item))))

  (it "increases the quality of Passes by three if < 5 and >= 0 days left"
    (doseq [item (update-quality (items [passes] 5 5))] ; will be 4 after sell-in dec
      (should= 8 (:quality item)))
    (doseq [item (update-quality (items [passes] 1 5))] ; will be 0 after sell-in dec
      (should= 8 (:quality item))))

  (it "drops the quality of Passes to zero when sell-in <= 0"
    (doseq [item (update-quality (items [passes] 0 5))] ; will be -1 after sell-in dec
      (should= 0 (:quality item))))

  (it "never increases quality more than 50"
    (doseq [item (update-quality (items [passes brie] 10 50))]
      (should= 50 (:quality item)))
    (doseq [item (update-quality (items [passes brie] 5 50))]
      (should= 50 (:quality item))))

  (it "udpates different items with different rules"
    (let [items (update-quality [(item vest 5 10)
                                 (item elixir 0 0)
                                 (item sulfuras 0 80)
                                 (item passes 5 30)
                                 (item brie 10 50)])]
      (should== [(item vest 4 9)
                 (item elixir -1 0)
                 (item sulfuras 0 80)
                 (item passes 4 33)
                 (item brie 9 50)]
                items))))
