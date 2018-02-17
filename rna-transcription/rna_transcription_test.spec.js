var toRna = require('./rna_transcription')

describe("toRna()", function() {
  it("transcribes cytosine to guanine", function() {
    expect(toRna('C')).toEqual('G')
  })

  it("transcribes guanine to cytosine", function() {
    expect(toRna('G')).toEqual('C')
  })

  it("transcribes adenine to uracil", function() {
    expect(toRna('A')).toEqual('U')
  })

  it("transcribes thymine to adenine", function() {
    expect(toRna('T')).toEqual('A')
  })

  it("transcribes all dna nucleotides to their rna complements", function() {
    expect(toRna('ACGTGGTCTTAA'))
      .toEqual('UGCACCAGAAUU')
  })
})
