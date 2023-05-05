module BookKeeping
  VERSION = 3
end

class Fixnum
  def divides?(numerator)
    numerator.remainder(self).zero?
  end
end

class Raindrops
  def self.convert(number)
    raindrops = ""

    if 3.divides?(number)
      raindrops += "Pling"
    end

    if 5.divides?(number)
      raindrops += "Plang"
    end

    if 7.divides?(number)
      raindrops += "Plong"
    end

    if raindrops.empty?
      "#{number}"
    else
      raindrops
    end
  end
end
