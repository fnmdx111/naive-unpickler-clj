(ns lein-test.core
  (:use [clojure.java.io :only [file input-stream]]))

(import '(java.io ByteArrayInputStream)
        '(java.lang StringBuffer String Integer))


(def cache-path "dc.dat")
(def opcode-hash (ref {}))

(defmacro defopcode [name key & forms]
  `(do (def ~name (int ~key))
       (dosync (alter opcode-hash
                      assoc-in [~key]
                      (fn [~'in-stream ~'stack ~'memo]
                        (let [~'next-byte #(.read ~'in-stream)]
                          ~@forms
                          [~'stack ~'memo]))))))



(defopcode PROTO 0x80 ;; protocol-version: 1byte arg
  (println "protocol version" (next-byte)))


(defopcode STOP \.
  (println "reached STOP"))


(defn -main []
  (println "Hello, World!")
  (with-open [is (input-stream cache-path)]
    (let [opcode (.read is)]
      (println (Integer/toHexString opcode))
      ((opcode-hash opcode) is [] {}))))


(defn process [in-stream]
  (loop [stack [] memo {}]
    (let [opcode (.read in-stream)
          dispatch-fn #((opcode-hash %) in-stream stack memo)]
      (if (and opcode
               (not= opcode STOP))
        (let [[stack memo] (dispatch-fn opcode)]
          (recur stack memo))
        (stack 0)))))