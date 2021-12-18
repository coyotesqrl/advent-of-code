;; ## Advent of Code 2021
;; ### Day 16: Packet Decoder
(ns coyotesqrl.2021.day16
  (:require [coyotesqrl.utils :as utils]))

;; ##### Input
(defn- line->bin [s]
  (->> s
       (map str)
       (map #(Integer/parseInt % 16))
       (map #(Integer/toBinaryString %))
       (map #(Integer/parseInt %))
       (map #(format "%04d" %))
       (apply str)))

(def day16-input (->> "coyotesqrl/2021/day16-input.txt"
                      utils/input->seq
                      first
                      line->bin))

;; #### Part 1
;; As you leave the cave and reach open waters, you receive a transmission from the Elves back on
;; the ship.
;;
;; The transmission was sent using the Buoyancy Interchange Transmission System (BITS), a method of
;; packing numeric expressions into a binary sequence. Your submarine's computer has saved the
;; transmission in hexadecimal (your puzzle input).
;;
;; The first step of decoding the message is to convert the hexadecimal representation into binary.
;; Each character of hexadecimal corresponds to four bits of binary data.
;;
;; The BITS transmission contains a single packet at its outermost layer which itself contains many
;; other packets. The hexadecimal representation of this packet might encode a few extra 0 bits at
;; the end; these are not part of the transmission and should be ignored.
;;
;; Every packet begins with a standard header: the first three bits encode the packet version, and
;; the next three bits encode the packet type ID. These two values are numbers; all numbers encoded
;; in any packet are represented as binary with the most significant bit first. For example, a
;; version encoded as the binary sequence 100 represents the number 4.
;;
;; Packets with type ID 4 represent a literal value. Literal value packets encode a single binary
;; number. To do this, the binary number is padded with leading zeroes until its length is a
;; multiple of four bits, and then it is broken into groups of four bits. Each group is prefixed by
;; a 1 bit except the last group, which is prefixed by a 0 bit. These groups of five bits
;; immediately follow the packet header.
;;
;; Every other type of packet (any packet with a type ID other than 4) represent an operator that
;; performs some calculation on one or more sub-packets contained within. Right now, the specific
;; operations aren't important; focus on parsing the hierarchy of sub-packets.
;;
;; An operator packet contains one or more packets. To indicate which subsequent binary data
;; represents its sub-packets, an operator packet can use one of two modes indicated by the bit
;; immediately after the packet header; this is called the length type ID:
;;
;; * If the length type ID is 0, then the next 15 bits are a number that represents the total length
;;   in bits of the sub-packets contained by this packet.
;; * If the length type ID is 1, then the next 11 bits are a number that represents the number of
;;   sub-packets immediately contained by this packet.
;; Finally, after the length type ID bit and the 15-bit or 11-bit field, the sub-packets appear.
(declare read-packet)
(declare read-tape)

(defn- bits->num [bits]
  (Long/parseLong (apply str bits) 2))

(defn- read-header [bits]
  (let [[ver typ] (->> bits
                       (partition 3)
                       (map #(apply str %))
                       (map #(Long/parseLong % 2))
                       (take 2))]
    {:version ver :type-id typ}))

(defn- get-lit-bits [bits drop?]
  (reduce (fn [{:keys [lit bits]} v]
            (let [lit (str lit (apply str v))
                  bits (drop 5 bits)
                  end (= \0 (first v))
                  bits (if (and drop? end)
                         (drop (quot (count lit) 5) bits)
                         bits)
                  acc {:lit lit :bits bits}]
              (if end
                (reduced acc)
                acc)))
          {:lit "" :bits bits}
          (partition 5 bits)))

(defn read-bit-length-subs
  [bits]
  (let [len (bits->num (take 15 bits))
        bits (drop 15 bits)
        sub-bits (take len bits)
        bits (drop len bits)]
    {:bits bits
     :sub-packets (read-tape {:bits sub-bits} false)}))

(defn- read-n-packets [bits n]
  (loop [n n
         bits bits
         resp (vector)]
    (if (zero? n)
      {:bits bits :sub-packets resp}
      (let [{:keys [bits packet]} (read-packet {:bits bits} false)]
        (recur (dec n) bits (conj resp packet))))))

(defn read-packet-num-subs
  [bits]
  (let [sub-cnt (bits->num (take 11 bits))
        bits (drop 11 bits)
        resp (read-n-packets bits sub-cnt)]
    resp))

(defn read-literal-value
  [packet bits outermost?]
  (let [{:keys [lit bits]} (get-lit-bits bits outermost?)
        lit (Long/parseLong (->> lit
                                 (partition 5)
                                 (map rest)
                                 flatten
                                 (apply str))
                            2)]
    {:bits bits
     :packet (assoc packet :lit-val lit)}))

(defn read-operator-envelope
  [packet bits]
  (let [env-type (first bits)
        bits     (rest bits)
        {:keys [sub-packets bits]} (if (= \0 env-type)
                                     (read-bit-length-subs bits)
                                     (read-packet-num-subs bits))]
    {:bits bits
     :packet (assoc packet :sub-packets sub-packets)}))

(defn read-packet
  [{:keys [bits]} outermost?]
  (let [{:keys [type-id] :as packet} (read-header bits)
        bits (drop 6 bits)]
    (if (= 4 type-id)
      (read-literal-value packet bits outermost?)
      (read-operator-envelope packet bits))))

;; ##### Slight over-engineering
;; I made an assumption early on that there would be multiple, top-level packets. Because of that,
;; I'd need a tape reader that iterated over packets. This does get used, by the
;; `read-bit-length-subs` fn, but I likely could have solved that in an other way.
;;
;; It's particularly frustrating because I have this operation, but am not able (easliy) to
;; repurpose it for the `read-packet-num-subs` fn. More ugliness.
(defn read-tape
  ([bits] (read-tape bits true))
  ([bits outermost?] (reduce (fn [a v]
                               (let [a (conj a (:packet v))]
                                 (if (or (empty? (:bits v)) (<= (count (:bits v)) 8))
                                   (reduced a)
                                   a)))
                             []
                             (iterate #(read-packet % outermost?) (read-packet bits outermost?)))))

(defn add-version-nums [in]
  (reduce (fn [a {:keys [version sub-packets]}]
            (if sub-packets
              (+ a version (add-version-nums sub-packets))
              (+ a version)))
          0
          in))

;; Decode the structure of your hexadecimal-encoded BITS transmission; **what do you get if you add
;; up the version numbers in all packets?**
(->> (read-tape {:bits day16-input})
     add-version-nums
     utils/answer-block)

;; ---
;; #### Part 2
;; Now that you have the structure of your transmission decoded, you can calculate the value of the
;; expression it represents.
;;
;; Literal values (`type ID 4`) represent a single number as described above. The remaining type IDs
;; are more interesting:
;;
;; * Packets with `type ID 0` are sum packets - their value is the sum of the values of their
;; sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
;; * Packets with `type ID 1` are product packets - their value is the result of multiplying together
;; the values of their sub-packets. If they only have a single sub-packet, their value is the value
;; of the sub-packet.
;; * Packets with `type ID 2` are minimum packets - their value is the minimum of the values of their
;; sub-packets.
;; * Packets with `type ID 3` are maximum packets - their value is the maximum of the values of their
;; sub-packets.
;; * Packets with `type ID 5` are greater than packets - their value is 1 if the value of the first
;; sub-packet is greater than the value of the second sub-packet; otherwise, their value is 0. These
;; packets always have exactly two sub-packets.
;; * Packets with `type ID 6` are less than packets - their value is 1 if the value of the first
;; sub-packet is less than the value of the second sub-packet; otherwise, their value is 0. These
;; packets always have exactly two sub-packets.
;; * Packets with `type ID 7` are equal to packets - their value is 1 if the value of the first
;; sub-packet is equal to the value of the second sub-packet; otherwise, their value is 0. These
;; packets always have exactly two sub-packets.
;; * Using these rules, you can now work out the value of the outermost packet in your BITS
;; transmission.

;; ##### Multi-method for operation types
(defmulti do-op :type-id)

;; ##### Helper functions to perform operations
(defn- perform-operation [packet op]
  (->> packet
       :sub-packets
       (map do-op)
       (apply op)))

(defn- perform-comparison [packet op]
  (let [[s1 s2] (:sub-packets packet)]
    (if (op (do-op s1) (do-op s2))
      1
      0)))

;; ##### Value packet
(defmethod do-op 4
  [packet]
  (:lit-val packet))

;; ##### Sum
(defmethod do-op 0
  [packet]
  (perform-operation packet +))

;; ##### Product
(defmethod do-op 1
  [packet]
  (perform-operation packet *))

;; ##### Min
(defmethod do-op 2
  [packet]
  (perform-operation packet min))

;; ##### Max
(defmethod do-op 3
  [packet]
  (perform-operation packet max))

;; ##### GT
(defmethod do-op 5
  [packet]
  (perform-comparison packet >))

;; ##### LT
(defmethod do-op 6
  [packet]
  (perform-comparison packet <))

;; ##### Equal To
(defmethod do-op 7
  [packet]
  (perform-comparison packet =))

;; **What do you get if you evaluate the expression represented by your hexadecimal-encoded BITS
;; transmission?**
(->> (read-tape {:bits day16-input})
     (map do-op)
     utils/answer-block)
