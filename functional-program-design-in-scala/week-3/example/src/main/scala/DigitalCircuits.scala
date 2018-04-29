
class DigitalCircuits {

}

trait Parameters {
  def InverterDelay = 2
  def AndDelay = 3
  def OrDelay = 5
}

abstract class Simulation {

  type Action = () => Unit

  case class Event(time: Int, action: Action)

  private type Agenda = List[Event]
  private var agenga: Agenda = List()
  var curTime: Int = 0

  def currentTime: Int = curTime

  def afterDelay(delay: Int)(block: () => Unit): Unit = {
    val event = Event(curTime + delay, block)
    agenga = insert(agenga, event)
  }

  def insert(agenga: Agenda, event: Event): List[Event] = agenga match {
    case first :: tail if first.time <= event.time =>
      first :: insert(tail, event)
    case _ => event :: agenga
  }

  def run(): Unit = {
    afterDelay(0) { () =>
      println(s"*** Simulation started, current time is $curTime ***")
    }
    loop()
  }

  private def loop(): Unit = agenga match {
    case first :: rest =>
      agenga = rest
      curTime = first.time
      first.action()
      loop()
    case Nil =>

  }

}

abstract class Gates extends Simulation {

  def InverterDelay: Int
  def AndDelay: Int
  def OrDelay: Int

  def inverter(input: Wire, output: Wire): Unit = {
    def inverterAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay){ () => output setSignal !inputSig}
    }
    input addAction inverterAction
  }

  def andGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      val in1Sig = in1.getSignal
      val in2sig = in2.getSignal
      afterDelay(AndDelay) { () => output setSignal (in1Sig & in2sig) }
    }
    in1 addAction andAction
    in2 addAction andAction
  }

  def orGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def orAction(): Unit = {
      val in1Sig = in1.getSignal
      val in2sig = in2.getSignal
      afterDelay(OrDelay) { () => output setSignal (in1Sig | in2sig) }
    }
    in1 addAction orAction
    in2 addAction orAction
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"Name=$name currentTime=$curTime value=${wire.getSignal}")
    }
    wire addAction probeAction
  }

  class Wire() {


    private var signal = false
    private var actions: List[Action] = List()

    def getSignal: Boolean = signal
    def setSignal(sig: Boolean): Unit = {
      if (sig != signal) {
        signal = sig
        actions.foreach(_())
      }

    }
    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }
  }

}

abstract class Circuits extends Gates {

  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = {
    val d, e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d,e,s)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, count: Wire): Unit = {
    val s, c1, c2 = new Wire
    halfAdder(a, cin, s, c1)
    halfAdder(b, s, sum, c2)
    orGate(c1, c2, count)
  }

}




