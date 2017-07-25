class Hamming
  def self.compute(strand1, strand2)
    unless strand1.length == strand2.length
      fail ArgumentError 'Mismatched strand lengths!'
    end

    # FIXME: gross
    difference = 0
    strand1.each_char.zip(strand2.each_char) { |nucleotide1,nucleotide2|
      difference += 1 unless nucleotide1 == nucleotide2
    }
    difference
  end
end
