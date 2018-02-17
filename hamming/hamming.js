exports.compute = function(a, b) {
  if (a.length != b.length) { throw 'DNA strands must be of equal length.' }
  return a.split('').reduce(function(distance, nucleotide, index) {
    return distance + ((nucleotide != b[index]) | 0)
  }, 0)
}
