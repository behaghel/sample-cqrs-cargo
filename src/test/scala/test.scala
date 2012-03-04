package cargo

import java.util.Date
import org.specs2.mutable._

class Test extends Specification {
  "Cargo" should {
    "demo" in {
      import model._
      import model.event._
      import service.CargoService._

      val SF = Port("San Francisco")
      val LA = Port("Los Angeles")

      val cargo = Cargo("Refactoring")

      val ship = Ship("King Roy", Some(LA), Set())
      Ships.add(ship)

      val movements = for {
        _ <- ship.load(cargo)
        _ <- ship.leave()
        f <- ship.arrive(at = SF)
      } yield f
      movements(ship) //log events, update ref in Repo

      ships.audit("King Roy").foreach(println)

      ship.port must beEqualTo(Some(SF))
    }
  }
}
