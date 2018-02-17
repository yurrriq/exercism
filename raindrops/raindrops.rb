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

    if 3.divides?(number) then
      raindrops += "Pling"
    end

    if 5.divides?(number) then
      raindrops += "Plang"
    end

    if 7.divides?(number) then
      raindrops += "Plong"
    end

    if raindrops.empty? then
      "#{number}"
    else
      raindrops
    end
  end
end
