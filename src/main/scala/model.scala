package cargo
package ddd {

  trait Entity[K] {
    type Key = K
    def id: K

    override def equals(other: Any): Boolean = other match {
      case that: Entity[_] => this.id == that.id // XXX should be an Entity of the *same* type as this
      case _ => false
    }
    override def hashCode(): Int = id.##
  }

  trait Repository[E <: Entity[_]] {
    def add(x: E): Option[E]
    def get(k: E#Key): Option[E]
  }
  
  package util {
    trait MapBasedRepository[E <: Entity[_]] extends Repository[E] {
      var all: Map[E#Key, E] = Map()
      def add(e: E): Option[E] =
        if (all.contains(e.id)) None else { all += (e.id -> e); Some(e) }
      def get(k: E#Key): Option[E] = all.get(k)
    }
  }

}

package model {

import java.util.Date
import scalaz._
import cargo.ddd._
import cargo.ddd.util._
import event._

case class Cargo(id: String) extends Entity[String]

case class Port(id: String) extends Entity[String]
case class Ship(id: String, port: Option[Port], cargos: Set[Cargo]) extends Entity[String] {
  def load(c: Cargo): State[Ship, LoadEvent] = {
    require(port.isDefined, "cannot load/unload a ship when not in a port")
    val now = new Date
    State(s => (LoadEvent(this, c, now), this.copy(cargos = this.cargos + c)))
  }
  def leave(): State[Ship, DepartureEvent] = {
    require(port.isDefined, "cannot leave from nowhere")
    val now = new Date
    State(s => (DepartureEvent(this, this.port.get, now), this.copy(port=None)))
  }
  def arrive(at: Port): State[Ship, ArrivalEvent] = {
    require(!port.isDefined, "must have left before arriving")
    val now = new Date
    State(s => (ArrivalEvent(this, at, now), this.copy(port=Some(at))))
  }
}
object Ships extends MapBasedRepository[Ship]


package event {
  sealed trait Event
  case class LoadEvent(ship: Ship, cargo: Cargo, date: Date) extends Event
  case class DepartureEvent(ship: Ship, port: Port, date: Date) extends Event
  case class ArrivalEvent(ship: Ship, port: Port, date: Date) extends Event
}
}
