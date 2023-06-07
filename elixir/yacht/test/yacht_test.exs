defmodule YachtTest do
  use ExUnit.Case

  test "Yacht" do
    assert Yacht.score(:yacht, [5, 5, 5, 5, 5]) == 50
  end

  test "Not Yacht" do
    assert Yacht.score(:yacht, [1, 3, 3, 2, 5]) == 0
  end

  test "Ones" do
    assert Yacht.score(:ones, [1, 1, 1, 3, 5]) == 3
  end

  test "Ones, out of order" do
    assert Yacht.score(:ones, [3, 1, 1, 5, 1]) == 3
  end

  test "No ones" do
    assert Yacht.score(:ones, [4, 3, 6, 5, 5]) == 0
  end

  test "Twos" do
    assert Yacht.score(:twos, [2, 3, 4, 5, 6]) == 2
  end

  test "Yacht counted as threes" do
    assert Yacht.score(:threes, [3, 3, 3, 3, 3]) == 15
  end

  test "Fours" do
    assert Yacht.score(:fours, [1, 4, 1, 4, 1]) == 8
  end

  test "Yacht of 3s counted as fives" do
    assert Yacht.score(:fives, [3, 3, 3, 3, 3]) == 0
  end

  test "Fives" do
    assert Yacht.score(:fives, [1, 5, 3, 5, 3]) == 10
  end

  test "Sixes" do
    assert Yacht.score(:sixes, [2, 3, 4, 5, 6]) == 6
  end

  test "Full house two small, three big" do
    assert Yacht.score(:full_house, [2, 2, 4, 4, 4]) == 16
  end

  test "Full house three small, two big" do
    assert Yacht.score(:full_house, [5, 3, 3, 5, 3]) == 19
  end

  test "Two pair is not a full house" do
    assert Yacht.score(:full_house, [2, 2, 4, 4, 5]) == 0
  end

  test "Four of a kind is not a full house" do
    assert Yacht.score(:full_house, [1, 4, 4, 4, 4]) == 0
  end

  test "Yacht is not a full house" do
    assert Yacht.score(:full_house, [2, 2, 2, 2, 2]) == 0
  end

  test "Four of a Kind" do
    assert Yacht.score(:four_of_a_kind, [6, 6, 4, 6, 6]) == 24
  end

  test "Yacht can be scored as Four of a Kind" do
    assert Yacht.score(:four_of_a_kind, [3, 3, 3, 3, 3]) == 12
  end

  test "Full house is not Four of a Kind" do
    assert Yacht.score(:four_of_a_kind, [3, 3, 3, 5, 5]) == 0
  end

  test "Little Straight" do
    assert Yacht.score(:little_straight, [3, 5, 4, 1, 2]) == 30
  end

  test "Little Straight as Big Straight" do
    assert Yacht.score(:big_straight, [1, 2, 3, 4, 5]) == 0
  end

  test "Four in order but not a little straight" do
    assert Yacht.score(:little_straight, [1, 1, 2, 3, 4]) == 0
  end

  test "No pairs but not a little straight" do
    assert Yacht.score(:little_straight, [1, 2, 3, 4, 6]) == 0
  end

  test "Minimum is 1, maximum is 5, but not a little straight" do
    assert Yacht.score(:little_straight, [1, 1, 3, 4, 5]) == 0
  end

  test "Big Straight" do
    assert Yacht.score(:big_straight, [4, 6, 2, 5, 3]) == 30
  end

  test "Big Straight as little straight" do
    assert Yacht.score(:little_straight, [6, 5, 4, 3, 2]) == 0
  end

  test "No pairs but not a big straight" do
    assert Yacht.score(:big_straight, [6, 5, 4, 3, 1]) == 0
  end

  test "Choice" do
    assert Yacht.score(:choice, [3, 3, 5, 6, 6]) == 23
  end

  test "Yacht as choice" do
    assert Yacht.score(:choice, [2, 2, 2, 2, 2]) == 10
  end
end
