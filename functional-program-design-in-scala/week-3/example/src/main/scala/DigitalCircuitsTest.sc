object MySimulation extends Circuits with Parameters

import MySimulation._

val in1, in2, sum, carry = new Wire

halfAdder(in1, in1, sum, carry)

probe("SUM", sum)
probe("CARRY", carry)

in1 setSignal true
run()