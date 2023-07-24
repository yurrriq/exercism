def build_tree:
  if .preorder | length == 0 then
    {}
  else
    (.preorder | first) as $v |
    (.inorder | index($v) + 1) as $pivot |
    {
      $v,
      l: (
        {
          preorder: .preorder[1:$pivot],
          inorder: .inorder[:$pivot]
        } | build_tree
      ),
      r: (
        {
          preorder: .preorder[$pivot:],
          inorder: .inorder[$pivot:]
        } | build_tree
      )
    }
  end
;

if (.preorder | length) != (.inorder | length) then
  "traversals must have the same length" | halt_error
elif (.preorder | sort) != (.inorder | sort) then
  "traversals must have the same elements" | halt_error
elif (.preorder | (sort != unique)) or (.inorder | (sort != unique)) then
  "traversals must contain unique items" | halt_error
else
  build_tree
end
