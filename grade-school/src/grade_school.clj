(ns grade-school
  "Altering and sorting grade school rosters."
  {:author "Eric Bailey"})

(defn add
  "Given a school, a student's name and a grade, append the student to the
  grade's roster, if not already present, and return the updated roster."
  [school student grade]
  (update school grade
          (fn [students]
            (cond
              (nil? students)            [student]
              (some #{student} students) students
              :else                      (conj students student)))))

(defn grade
  "Given a school and a grade, return the list of students in the given grade
  at the given school."
  [school grade]
  (get school grade []))

(defn sorted
  "Given a school, sort it by grade and then by student and return the result."
  [school]
  (into (sorted-map) (for [[k v] school] [k (sort v)])))
