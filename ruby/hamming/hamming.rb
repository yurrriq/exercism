class Hamming
  def self.compute(strand1, strand2)
    unless strand1.length == strand2.length
      fail ArgumentError 'Mismatched strand lengths!'
    end

    # FIXME: three traversals...
    strand1.each_char
      .zip(strand2.each_char)
      .map { |pair| pair.reduce(:==) }
      .count(false)
  end
end
