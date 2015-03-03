(ns structured-data)

(defn do-a-thing
  "Returns x raised to the power of x."
  [x]
  (let [n (+ x x)]
    (Math/pow n n)))

(defn spiff
  "Returns the sum of the first and third elements of a given vector."
  [v]
  (let [first (get v 0)
        third (get v 2)]
    (+ first third)))


(defn cutify
  "Accepts a vecotr and adds '<3' to its end."
  [v]
  (conj v "<3"))

(defn spiff-destructuring
  "Returns the sum of the first and third elements of a given vector."
  [v]
  (let [[a _ c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width
  "Given a rectangle, returns its width."
  [rectangle]
  (let [[p1 p2] rectangle
        [x1 y1] p1
        [x2 y2] p2
        abs (fn [x]
              (if (pos? x) x (- x)))]
    (abs (- x1 x2))))

(defn height
  "Given a rectangle, returns its height."
  [rectangle]
  (let [[p1 p2] rectangle
        [x1 y1] p1
        [x2 y2] p2
        abs (fn [x]
              (if (pos? x) x (- x)))]
    (abs (- y1 y2))))

(defn square?
  "Returns true if rectangle is a square."
  [rectangle]
  (= (height rectangle)
     (width rectangle)))


(defn area
  "Returns area of a rectangle."
  [rectangle]
  (* (height rectangle)
     (width rectangle)))

(defn contains-point?
  "Returns true if rectangle contains point."
  [rectangle point]
  (let [[p1 p2] rectangle
        [x1 y1] p1
        [x2 y2] p2
        [x3 y3] point]
    (and (or (<= x1 x3 x2)
             (<= x2 x3 x1))
         (or (<= y1 y3 y2)
             (<= y2 y3 y1)))))

(defn contains-rectangle?
  "Returns true if inner rectangle is inside of outer rectangle."
  [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))

(defn title-length
  "Counts the length of a books title."
  [book]
  (count (:title book)))

(defn author-count
  "Counts the number of authors to a given book."
  [book]
  (count (:authors book)))

(defn multiple-authors?
  "Returns true if a book has multiple authors."
  [book]
  (> (author-count book) 1))

(defn add-author
  "Adds a new author to a book."
  [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn author-names
  "Returns all of the author names for a given book"
  [book]
  (map :name (:authors book)))

(defn all-author-names
  "Given a list of books, returns all of the unique authors."
  [books]
  (set (apply concat (map author-names books))))

(defn alive? [author]
  (not (boolean (:death-year author))))

(defn element-lengths
  "Returns the lengths of every item in collection."
  [collection]
  (map count collection))

(defn second-elements
  "Takes a vector of vectors and returns a sequence of the second elements."
  [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)))

(defn titles
  "Takes a sequence of books and returns their titles."
  [books]
  (let [get-title (fn [x] (:title x))]
   (map get-title books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars
  "Returns a string with n asterisks."
  [n]
  (apply str (repeat n "*")))

(defn toggle
  "Removes or adds an element from a set."
  [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq))
          (count a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map (fn [x] (:authors x)) books)))

(defn all-author-names [books]
  (let [get-name (fn [x] (:name x))]
    (set (map get-name (authors books)))))

(defn author->string [author]
  (let [name (str (:name author))
        lifespan (str "(" (:birth-year author) " - " (:death-year author) ")")]
    (if (:birth-year author)
      (str name " " lifespan)
      (str name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (str (:title book))
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))

(defn books->string [books]
  (let [num (count books)
        book-names (apply str (interpose ". " (map book->string books)))]
    (cond (= num 0) (str "No books.")
          (= num 1) (str "1 book. " book-names ".")
          :else (str num " books. " book-names "."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [matches (filter (fn [x] (= (:name x) name)) authors)]
    (if (pos? (count matches))
      (first matches)
      nil)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [matches (living-authors (:authors book))]
    (pos? (count matches))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
