compile: robot_simulator.erl robot_simulator_tests.erl
	erlc $^

test: compile
	erl -noshell -eval "eunit:test(robot_simulator_tests, [verbose])" -s init stop

.PHONY: doc
doc:
	erl -noshell -run edoc_run files '["robot_simulator.erl"]' '[{dir, "./doc"}]'
