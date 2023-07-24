# Add the elements of the input array, and return the sum.
#
# Example:
#   [1, 2, 3] | array_add           # => 6

def array_add:
  if . == [] then
    0
  else
    .[0] + (.[1:] | array_add)
  end
;

# Reverse the input array, and return the result in a new array.
#
# Example:
#   [1, 2, 3] | array_reverse       # => [3, 2, 1]

def array_reverse:
  if . == [] then
    []
  else
    (.[1:] | array_reverse) + [.[0]]
  end
;

# Run the filter `f` for each element of the input array,
# and return the outputs in a new array.
#
# Example:
#   [1, 2, 3] | array_map(. + 1)    # => [2, 3, 4]

def array_map(f):
  if . == [] then
    []
  else
    [.[0] | f] + (.[1:] | array_map(f))
  end
;
