var map          = {A: 'U', C: 'G', G: 'C', T: 'A'},
    throwInvalid = function(nucleotide) {
      throw new Error('Invalid nucleotide: ' + nucleotide)
    }


module.exports = function (strand) {
  return strand.replace(/./g, function(nucleotide) {
    return map[nucleotide] || throwInvalid(nucleotide)
  })
}
