#!/usr/bin/env bats
# generated on 2022-11-02T20:59:40Z
load bats-extra

@test 'Brown and black' {
    run jq -r -f resistor-color-duo.jq << 'END_INPUT'
        {
          "colors": [
            "brown",
            "black"
          ]
        }
END_INPUT

    assert_success
    expected=10
    assert_equal "$output" "$expected"
}

@test 'Blue and grey' {
    run jq -r -f resistor-color-duo.jq << 'END_INPUT'
        {
          "colors": [
            "blue",
            "grey"
          ]
        }
END_INPUT

    assert_success
    expected=68
    assert_equal "$output" "$expected"
}

@test 'Yellow and violet' {
    run jq -r -f resistor-color-duo.jq << 'END_INPUT'
        {
          "colors": [
            "yellow",
            "violet"
          ]
        }
END_INPUT

    assert_success
    expected=47
    assert_equal "$output" "$expected"
}

@test 'White and red' {
    run jq -r -f resistor-color-duo.jq << 'END_INPUT'
        {
          "colors": [
            "white",
            "red"
          ]
        }
END_INPUT

    assert_success
    expected=92
    assert_equal "$output" "$expected"
}

@test 'Orange and orange' {
    run jq -r -f resistor-color-duo.jq << 'END_INPUT'
        {
          "colors": [
            "orange",
            "orange"
          ]
        }
END_INPUT

    assert_success
    expected=33
    assert_equal "$output" "$expected"
}

@test 'Ignore additional colors' {
    run jq -r -f resistor-color-duo.jq << 'END_INPUT'
        {
          "colors": [
            "green",
            "brown",
            "orange"
          ]
        }
END_INPUT

    assert_success
    expected=51
    assert_equal "$output" "$expected"
}

@test 'Black and brown, one-digit' {
    run jq -r -f resistor-color-duo.jq << 'END_INPUT'
        {
          "colors": [
            "black",
            "brown"
          ]
        }
END_INPUT

    assert_success
    expected=1
    assert_equal "$output" "$expected"
}

