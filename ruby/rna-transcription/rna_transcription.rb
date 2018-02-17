module BookKeeping
  VERSION = 4
end

class Complement
  def self.of_dna(strand)
    unless strand =~ /[^ACGT]/
      strand.tr('ACGT', 'UGCA')
    else
      ''
    end
  end
end
