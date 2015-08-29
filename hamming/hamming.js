exports.compute = function(a, b) {
  if (a.length != b.length) { throw 'DNA strands must be of equal length.' }
  var bs = b.split('')
  return a.split('').reduce(function(distance, nucleotide, index, _array) {
    return distance + ((nucleotide != bs[index]) | 0)
  }, 0)
}
