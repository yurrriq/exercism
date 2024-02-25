datatype allergen =
  Eggs
| Peanuts
| Shellfish
| Strawberries
| Tomatoes
| Chocolate
| Pollen
| Cats

(*!
 * fromEnum allergen coverts an allergen to an int.
 *)
fun fromEnum Eggs = 0
  | fromEnum Peanuts = 1
  | fromEnum Shellfish = 2
  | fromEnum Strawberries = 3
  | fromEnum Tomatoes = 4
  | fromEnum Chocolate = 5
  | fromEnum Pollen = 6
  | fromEnum Cats = 7


local open Word infix 8 >> infix 7 andb
in
  (*!
   * isAllergicTo score allergen determines if score indicates an allergy to allergen.
   *)
  fun isAllergicTo i a =
    let
      fun testBit w j =
        (0wx1 = ((w >> (Word.fromInt j)) andb 0wx1))
    in
      testBit (Word.fromInt i) (fromEnum a)
    end
end


fun allergies n =
  List.filter (isAllergicTo n)
    [Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats]
