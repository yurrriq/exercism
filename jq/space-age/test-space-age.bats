#!/usr/bin/env bats
# generated on 2022-11-02T20:59:49Z
load bats-extra

@test 'age on Earth' {
    run jq -r -f space-age.jq << 'END_INPUT'
        {
          "planet": "Earth",
          "seconds": 1000000000
        }
END_INPUT

    assert_success
    expected=31.69
    assert_equal "$output" "$expected"
}

@test 'age on Mercury' {
    run jq -r -f space-age.jq << 'END_INPUT'
        {
          "planet": "Mercury",
          "seconds": 2134835688
        }
END_INPUT

    assert_success
    expected=280.88
    assert_equal "$output" "$expected"
}

@test 'age on Venus' {
    run jq -r -f space-age.jq << 'END_INPUT'
        {
          "planet": "Venus",
          "seconds": 189839836
        }
END_INPUT

    assert_success
    expected=9.78
    assert_equal "$output" "$expected"
}

@test 'age on Mars' {
    run jq -r -f space-age.jq << 'END_INPUT'
        {
          "planet": "Mars",
          "seconds": 2129871239
        }
END_INPUT

    assert_success
    expected=35.88
    assert_equal "$output" "$expected"
}

@test 'age on Jupiter' {
    run jq -r -f space-age.jq << 'END_INPUT'
        {
          "planet": "Jupiter",
          "seconds": 901876382
        }
END_INPUT

    assert_success
    expected=2.41
    assert_equal "$output" "$expected"
}

@test 'age on Saturn' {
    run jq -r -f space-age.jq << 'END_INPUT'
        {
          "planet": "Saturn",
          "seconds": 2000000000
        }
END_INPUT

    assert_success
    expected=2.15
    assert_equal "$output" "$expected"
}

@test 'age on Uranus' {
    run jq -r -f space-age.jq << 'END_INPUT'
        {
          "planet": "Uranus",
          "seconds": 1210123456
        }
END_INPUT

    assert_success
    expected=0.46
    assert_equal "$output" "$expected"
}

@test 'age on Neptune' {
    run jq -r -f space-age.jq << 'END_INPUT'
        {
          "planet": "Neptune",
          "seconds": 1821023456
        }
END_INPUT

    assert_success
    expected=0.35
    assert_equal "$output" "$expected"
}

@test 'invalid planet causes error' {
    run jq -r -f space-age.jq << 'END_INPUT'
        {
          "planet": "Sun",
          "seconds": 680804807
        }
END_INPUT

    assert_failure
    expected='not a planet'
    assert_equal "$output" "$expected"
}

