#!/usr/bin/env bats
# generated on 2022-11-02T20:58:55Z
load bats-extra

@test 'no matches' {
    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "diaper",
          "candidates": [
            "hello",
            "world",
            "zombies",
            "pants"
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'detects two anagrams' {
    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "solemn",
          "candidates": [
            "lemons",
            "cherry",
            "melons"
          ]
        }
END_INPUT

    assert_success
    expected='["lemons","melons"]'
    assert_equal "$output" "$expected"
}

@test 'does not detect anagram subsets' {
    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "good",
          "candidates": [
            "dog",
            "goody"
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'detects anagram' {
    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "listen",
          "candidates": [
            "enlists",
            "google",
            "inlets",
            "banana"
          ]
        }
END_INPUT

    assert_success
    expected='["inlets"]'
    assert_equal "$output" "$expected"
}

@test 'detects three anagrams' {
    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "allergy",
          "candidates": [
            "gallery",
            "ballerina",
            "regally",
            "clergy",
            "largely",
            "leading"
          ]
        }
END_INPUT

    assert_success
    expected='["gallery","regally","largely"]'
    assert_equal "$output" "$expected"
}

@test 'detects multiple anagrams with different case' {
    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "nose",
          "candidates": [
            "Eons",
            "ONES"
          ]
        }
END_INPUT

    assert_success
    expected='["Eons","ONES"]'
    assert_equal "$output" "$expected"
}

@test 'does not detect non-anagrams with identical checksum' {
    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "mass",
          "candidates": [
            "last"
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'detects anagrams case-insensitively' {
    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "Orchestra",
          "candidates": [
            "cashregister",
            "Carthorse",
            "radishes"
          ]
        }
END_INPUT

    assert_success
    expected='["Carthorse"]'
    assert_equal "$output" "$expected"
}

@test 'detects anagrams using case-insensitive subject' {
    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "Orchestra",
          "candidates": [
            "cashregister",
            "carthorse",
            "radishes"
          ]
        }
END_INPUT

    assert_success
    expected='["carthorse"]'
    assert_equal "$output" "$expected"
}

@test 'detects anagrams using case-insensitive possible matches' {
    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "orchestra",
          "candidates": [
            "cashregister",
            "Carthorse",
            "radishes"
          ]
        }
END_INPUT

    assert_success
    expected='["Carthorse"]'
    assert_equal "$output" "$expected"
}

@test 'does not detect an anagram if the original word is repeated' {
    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "go",
          "candidates": [
            "go Go GO"
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'anagrams must use all letters exactly once' {
    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "tapper",
          "candidates": [
            "patter"
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'words are not anagrams of themselves' {
    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "BANANA",
          "candidates": [
            "BANANA"
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'words are not anagrams of themselves even if letter case is partially different' {
    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "BANANA",
          "candidates": [
            "Banana"
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'words are not anagrams of themselves even if letter case is completely different' {
    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "BANANA",
          "candidates": [
            "banana"
          ]
        }
END_INPUT

    assert_success
    expected='[]'
    assert_equal "$output" "$expected"
}

@test 'words other than themselves can be anagrams' {
    run jq -c -f anagram.jq << 'END_INPUT'
        {
          "subject": "LISTEN",
          "candidates": [
            "LISTEN",
            "Silent"
          ]
        }
END_INPUT

    assert_success
    expected='["Silent"]'
    assert_equal "$output" "$expected"
}

