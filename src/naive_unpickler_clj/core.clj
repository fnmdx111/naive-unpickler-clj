(ns naive-unpickler-clj.core
  (:use [clojure.java.io :only [input-stream]]))

(import '(java.io StringReader))
(import '(java.nio ByteBuffer
                   ByteOrder)
        '(java.io StringReader)
        '(org.apache.commons.lang StringEscapeUtils)
        '(java.lang Double))


(def -marker (Object.))
(def opcode-hash (ref {}))


(defn readline [in-stream]
  (loop [result []
         current-byte (.read in-stream)]
    (if (= (int \newline) current-byte)
      result
     (recur (conj result current-byte) (.read in-stream)))))


(defmacro defopcode [name key & forms]
  `(do (def ~name (int ~key))
       (dosync (alter opcode-hash
                      assoc-in [(int ~key)]
                      (fn [~'in-stream ~'stack ~'memo]
                        (let [~'next-byte #(.read ~'in-stream)
                              ~'next-line #(readline ~'in-stream)
                              ~'next-n-byte #(apply vector (map (fn [_#] (.read ~'in-stream)) (range %)))
                              ~'next-4-byte #(~'next-n-byte 4)] 
                          ~@forms))))))



(defopcode PROTO \u0080 ;; protocol version: 1byte arg
  (println "protocol version" (next-byte))
  [stack memo])


(defopcode STOP \. ;; stop pickling machine: no arg
  (println "reached STOP")
  [stack memo])


(def TRUE [0x01])
(def FALSE [0x00])

(defn from-byte-vector-lendian [vec]
  (reduce (fn [acc n] (-> acc (#(bit-shift-left % 8)) (#(+ % n))))
          0 (reverse vec)))


(defopcode INT \I ;; INT: till newline char
  (let [data (next-line)]
    (cond (= TRUE data) [(conj stack true) memo]
          (= FALSE data) [(conj stack false) memo]
          :else [(conj stack
                       (Integer/valueOf (apply str
                                               (map char data))))
                 memo])))
;; (t-unpickle [0x80 2 \I 0x31 0x32 0x33 0x34 0x35 0x36 0x0a \.])
;; 123456

(defopcode BININT \J ;; BININT: 4byte arg, push a four-byte signed int
  (let [data (next-4-byte)] ;; read next 4 bytes
    [(conj stack (from-byte-vector-lendian data)) memo]))
;; (t-unpickle [0x80 2 \J 0x40 0xe2 1 0 \.])
;; 123456

(defopcode BININT1 \K ;; BININT1: 1byte arg, push a one-byte unsigned int
  (let [data [(next-byte)]]
    [(conj stack (from-byte-vector-lendian data)) memo]))
;; (t-unpickle [0x80 2 \K 0x0a \.])
;; 10

(defopcode BININT2 \M ;; BININT2: 2byte arg, push a two-byte unsigned int
  (let [data [(next-byte) (next-byte)]]
    [(conj stack (from-byte-vector-lendian data)) memo]))
;; (t-unpickle [0x80 2 \M 0x0a 0x0a \.])
;; 2570

(defopcode LONG \L ;; LONG: till newline char, same as INT, except result ends with a L
  ((opcode-hash (int \I)) in-stream stack memo))
;; (t-unpickle [0x80 2 \L 0x31 0x32 0x33 0x34 0x35 0x36 0x0a \.])
;; 123456

(defopcode LONG1 \u008a ;; LONG1: a long with length of 1byte
  (let [len (next-byte)
        data (next-n-byte len)]
    [(conj stack (from-byte-vector-lendian data)) memo]))
;; (t-unpickle [0x80 2 0x8a 2 0xd2 4 \.])
;; 12343

(defopcode LONG4 \u008b
  (let [len (from-byte-vector-lendian (next-4-byte))
        data (next-n-byte len)]
    [(conj stack (from-byte-vector-lendian data)) memo]))
;; (t-unpickle [0x80 2 0x8b 1 0 0 0 0x0a \.])
;; 10

(defopcode STRING \S
  (let [-d (next-line)
        data (cond
              (or (= (first -d) (int \'))
                  (= (first -d) (int \"))) (-> -d rest drop-last)
              :else -d)]
    [(conj stack (-> data
                     (#(map char %))
                     (#(apply str %))
                     StringEscapeUtils/unescapeJava))
     memo]))
;; (t-unpickle (map int "S'asdf\\n'\np0\n."))
;; "asdf\\n"

(defopcode PUT \p
  (let [idx (apply str (map char (next-line)))]
    [stack (assoc memo idx (last stack))]))
;; (t-unpickle (map int "S'asdf\\n'\np0\n."))
;; "asdf\n"

(defopcode BINPUT \q
  (let [idx (str (next-byte))]
    [stack (assoc memo idx (last stack))]))
;; (t-unpickle (map int "S'asdf\\n'\nq\u0000."))
;; "asdf\n"

(defopcode LONG_BINPUT \r
  (let [idx (str (from-byte-vector-lendian (next-4-byte)))]
    [stack (assoc memo idx (last stack))]))

(defopcode BINSTRING \T
  (let [len (from-byte-vector-lendian (next-4-byte))
        data (next-n-byte len)]
    [(conj stack (apply str (map char data))) memo]))
;; (t-unpickle [0x80 2 \T 6 0 0 0 0x61 0x73 0x64 0x66 0x99 0x0a \.])
;; "asdf\u0099\n"

(defopcode SHORT_BINSTRING \U
  (let [len (from-byte-vector-lendian [(next-byte)])
        data (next-n-byte len)]
    [(conj stack (apply str (map char data))) memo]))
;; (t-unpickle [0x80 2 \U 6 0x61 0x73 0x64 0x66 0x99 0x0a \.])
;; "asdf\u0099\n"

(defopcode NONE \N
  [(conj stack nil) memo])
;; (t-unpickle [0x80 1 \N \.])
;; nil

(defopcode NEWTRUE \u0088
  [(conj stack true) memo])
;; (t-unpickle [0x80 2 0x88 \.])
;; true

(defopcode NEWFALSE \u0089
  [(conj stack false) memo])
;; (t-unpickle [0x80 0x02 0x89 \.])
;; false

(defopcode UNICODE \V
  (let [data (next-line)]
    [(conj stack (-> data
                     (#(map char %))
                     (#(apply str %))
                     StringEscapeUtils/unescapeJava))
     memo]))
;; (t-unpickle (map int "V\\u54c8\\u54c8\\u000a\u0099\np0\n."))
;; "哈哈\n\u0099"

(defn to-java-byte-array [data]
  (byte-array (map #(byte (if (> % 128)
                            (- % 0x100)
                            %))
                   data)))


(defopcode BINUNICODE \X
  (let [len (from-byte-vector-lendian (next-4-byte))
        data (next-n-byte len)]
    [(conj stack
           (String. (to-java-byte-array data)
                    "utf-8"))
     memo]))
;; (t-unpickle [0x80 2 \X \tab 0 0 0 0xe5 0x93 0x88 0xe5 0x93 0x88 0x0a 0xc2 0x99 \q 0 \.])
;; "哈哈\n\u0099"

(defopcode FLOAT \F
  (let [data (next-line)]
    [(conj stack (Double/valueOf (apply str (map char data))))
     memo]))
;; (t-unpickle (map int "F123.234\n."))
;; 123.234

(defopcode BINFLOAT \G
  (let [data (next-n-byte 8)
        float-number (.. ByteBuffer (wrap (to-java-byte-array data)) (getDouble))]
    [(conj stack float-number) memo]))
;; (t-unpickle [0x80 2 \G \@ \^ 0xce 0xf9 0xdb \" 0xd0 0xe5 \.])
;; 123.234

(defopcode EMPTY_LIST \]
  [(conj stack []) memo])

(defopcode APPEND \a
  (let [[stack [top item]] (split-at (- (count stack) 2) stack)
        stack (into [] stack)]
    [(conj stack (conj top item)) memo]))
;; (t-unpickle [0x80 2 \] \q 0 \K 1 \a \.])
;; [1]

(defn split-top [coll] ;; `top' could be nil
  (let [[coll [top]] (split-at (- (count coll) 1) coll)]
    [(into [] coll) top]))

(defn split-at-marker [coll]
  (let [marker-pos (- (count coll) 1 (.indexOf (rseq coll) -marker))
        [stack [marker & coll]] (split-at marker-pos coll)
        [stack top] (split-top stack)] ;; get top element
    [stack top (if (nil? coll) [] coll)]))

(defopcode APPENDS \e
  (let [[stack top coll] (split-at-marker stack)]
    [(conj stack (into [] (concat top coll))) memo]))
;; (t-unpickle [0x80 2 \] \q 0 \( \K 1 \U 1 \a \q 1 \e \.])
;; [1 "a"]

(defopcode MARK \(
  [(conj stack -marker) memo])

(defn split-at-marker-without-top [stack]
  (let [[stack top coll] (split-at-marker stack)
        stack (if (nil? top) stack (conj stack top))]
    [stack coll]))

(defopcode LIST \l
  (let [[stack coll] (split-at-marker-without-top stack)]
    [(conj stack coll) memo]))
;; (t-unpickle (map int "(lp0\nI1\naS'a'\np1\na."))
;; [1 "a"]

;; Note that tuples can be regarded as frozen lists, so
;; they will be mapped to vectors as well.
(defopcode EMPTY_TUPLE \) ;; same as EMPTY_LIST
  ((opcode-hash (int \])) in-stream stack memo))

(defopcode TUPLE \t ;; same as LIST
  ((opcode-hash (int \l)) in-stream stack memo))

(defopcode TUPLE1 \u0085 ;; it is guaranteed that top will not be nil by pickletools.py line 1214
  (let [[stack top] (split-top stack)]
    [(conj stack [top]) memo]))
;; (t-unpickle [0x80 2 \K 1 0x85 \q 0 \.])
;; [1]

(defn split-at-last-n-item [n coll]
  (let [len (count coll)
        n (min n len)]
    (map #(into [] %)
         (split-at (- (count coll) n) coll))))

(defopcode TUPLE2 \u0086
  (let [[stack last-two-items] (split-at-last-n-item 2 stack)]
    [(conj stack last-two-items) memo]))
;; (t-unpickle [0x80 2 \K 1 \U 1 \2 \q 0 0x86 \q 1 \.])
;; [1 "2"]

(defopcode TUPLE3 \u0087
  (let [[stack last-three-items] (split-at-last-n-item 3 stack)]
    [(conj stack last-three-items) memo]))
;; (t-unpickle [0x80 2 \K 1 \K 2 \] \q 0 \U 1 \a \q 1 \a 0x87 \q 2 \.])
;; [1 2 ["a"]]

(defopcode EMPTY_DICT \}
  [(conj stack {}) memo])
;; (t-unpickle [0x80 2 \} \q 0 \K 1 \K 2 \s \.])
;; {1 2}

(defopcode DICT \d
  (let [[stack coll] (split-at-marker-without-top stack)]
    [(conj stack (apply hash-map coll)) memo]))
;; (t-unpickle (map int "(dp0\nI1\nI2\ns."))
;; {1 2}

(defopcode SETITEM \s
  (let [[stack [d key value]] (split-at-last-n-item 3 stack)]
    [(conj stack (assoc d key value)) memo]))
;; test case is the same as the previous two

(defopcode SETITEMS \u
  (let [[stack top coll] (split-at-marker stack)]
    [(conj stack (conj top (apply hash-map coll))) memo]))
;; (t-unpickle [0x80 2 \} \q 0 \( \K 1 \K 2 \K 2 \K 3 \K 3 \K 4 \u \.])
;; {3 4, 2 3, 1 2}

(defopcode POP \0
  [(into [] (drop-last stack)) memo])

(defopcode DUP \2
  (let [top (last stack)]
    [(conj stack top) memo]))

(defopcode POP_MARK \1
  (let [[stack coll] (split-at-marker-without-top stack)]
    [stack memo]))

(defopcode GET \g
  (let [idx (apply str (map char (next-line)))]
    [(conj stack (memo idx)) memo]))
;; (t-unpickle (map int "(lp0\nS'ab'\np1\naS'cd'\np2\nag1\na."))
;; ["ab" "cd" "ab"]

(defopcode BINGET \h
  (let [idx (str (next-byte))]
    [(conj stack (memo idx)) memo]))
;; (t-unpickle [0x80 2 \] \q 0 \( \U 2 \a \b \q 1 \U 2 \c \d \q 2 \h 1 \e \.])
;; ["ab" "cd" "ab"]

(defopcode LONG_BINGET \j
  (let [idx (str (from-byte-vector-lendian (next-4-byte)))]
    [(conj stack (memo idx)) memo]))


;;(defn opcode-not-found [in-stream stack memo]
;;  (println "certain opcode not found, please check your input")
;;  (throw (IllegalArgumentException.)))

(defn unpickle [in-stream]
  (loop [stack [] memo {}]
    (let [opcode (.read in-stream)
          dispatch-fn #((opcode-hash %) in-stream stack memo)]
      (println "opcode" (char opcode))
      (if (and opcode
               (not= opcode STOP))
        (let [[stack memo] (dispatch-fn opcode)]
          (recur stack memo))
        (stack 0)))))

(defn create-dummy-istream [vec]
  (StringReader. (apply str (map char vec))))

(defn t-unpickle [vec] ;; test only
  (let [vec (map #((if (= (type %) Character) int identity) %) vec)]
    (-> vec create-dummy-istream unpickle)))


(defn -main []
  (with-open [is (input-stream "dc.dat")]
    (unpickle is)))

