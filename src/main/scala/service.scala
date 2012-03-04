package cargo
package service

import model._

trait CargoService {
  import model.event._

  trait EntitySupport[T, K] {
    def add(x: T): Option[T]
    def query(k: K): Option[T]
  }

  trait CargoSupport extends EntitySupport[Cargo, String]

  trait PortSupport extends EntitySupport[Port, String]

  trait ShipSupport extends EntitySupport[Ship, String] {
    def audit(id: String): Seq[Event]

    def load(e: LoadEvent): Option[Ship] =
      if (!e.ship.cargos.contains(e.cargo)) Some(e.ship.copy(cargos = e.ship.cargos + e.cargo))
      else None

    def departure(e: DepartureEvent): Option[Ship] =
      if (e.ship.port.exists(_ == e.port)) Some(e.ship)
      else None

    def arrival(e: ArrivalEvent): Option[Ship] = 
      if (e.ship.port.exists( _ !=  e.port)) Some(e.ship.copy(port = Some(e.port)))
      else None
  }

  val cargos: CargoSupport
  val ports: PortSupport
  val ships: ShipSupport
}

object CargoService extends CargoService {
  import model.event._

  override val cargos = new CargoSupport {
    override def add(cargo: Cargo) =
      if (!states.contains(cargo.id)) {
        states += cargo.id -> cargo
        Some(cargo)
      } else None

    override def query(id: String) = states.get(id)

    private var states = Map[String, Cargo]()
  }

  override val ports = new PortSupport {
    override def add(port: Port) =
      if (!states.contains(port.id)) {
        states += port.id -> port
        Some(port)
      } else None

    override def query(id: String) = states.get(id)

    private var states = Map[String, Port]()
  }

  override val ships = new ShipSupport {
    override def add(ship: Ship) = {
      events += ship.id -> Nil
      if (!states.contains(ship.id)) {
        states += ship.id -> ship
        Some(ship)
      } else None
    }

    override def query(id: String) = states.get(id)

    override def audit(id: String) = events.get(id).getOrElse(Nil)

    override def load(e: LoadEvent) = super.load(e).map(log(e))
    override def departure(e: DepartureEvent) = super.departure(e).map(log(e))
    override def arrival(e: ArrivalEvent) = super.arrival(e).map(log(e))

    private def log(e: Event)(ship: Ship) = {
      events.get(ship.id).foreach(xs => events += ship.id -> (xs :+ e))
      states -= ship.id
      states += ship.id -> ship
      ship
    }

    private var states = Map[String, Ship]()
    private var events = Map[String, Seq[Event]]()
  }
}
