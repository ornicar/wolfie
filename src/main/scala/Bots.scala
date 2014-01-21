package gainsbar

import Dir._
import scala.util.Random
import Tile._

trait Bot {
  def move(input: Input): Dir
}

class RandomBot extends Bot {

  def move(input: Input) = {
    Random.shuffle(List(Dir.North, Dir.South, Dir.East, Dir.West)) find { dir ⇒
      input.game.board at input.hero.pos.to(dir) exists (Wall!=)
    }
  } getOrElse Dir.Stay
}

class GainsBot extends Bot {

  import GainsBot._

  def move(input: Input) = {
    val helper = Helper(input)
    import input._
    import helper._

    val tavernPath = ->(hero.pos, isTavern)
    val minePath = ->(hero.pos, isNewMine)

    def aybabtu = minePath.isEmpty
    def safeToGoMining = hero.life + mineLife + minePath.size * dayLife > 5

    val path =
      if (tavernPath.size == 1 && hero.life < 80) tavernPath
      else if (aybabtu) {
        if (tavernPath.size > 1) tavernPath
        else Nil
      }
      else if (safeToGoMining) minePath
      else tavernPath

    ~path.headOption.flatMap(hero.pos.dirTo)
  }
}

object GainsBot {

  val maxLife = 100
  val beerLife = 50
  val beerGold = -2
  val dayLife = -1
  val mineLife = -20
  val attackLife = -0
  val defendLife = -20
}

case class Helper(input: Input) {

  import input._

  def isTavern(pos: Pos) = is(pos, Tile.Tavern==)
  def isNewMine(pos: Pos) = is(pos, {
    case Tile.Mine(h) ⇒ h != Some(hero.id)
    case _            ⇒ false
  })
  def isAir(pos: Pos) = is(pos, Tile.Air==)
  def is(pos: Pos, f: Tile ⇒ Boolean) = game.board at pos exists f

  // shortest path to a tile satisfying goal function
  def ->(from: Pos, goal: Pos ⇒ Boolean): Path = {
    @annotation.tailrec
    def step(paths: Vector[Path], visited: Set[Pos]): Path = paths match {
      case Vector() ⇒ Nil
      case path +: rest ⇒ {
        val on = path.head
        if (visited(on)) step(rest, visited) else {
          on.neighbors find goal match {
            case Some(found) ⇒ (found :: path).reverse drop 1
            case None ⇒
              val nextPaths = (airNeighbors(on) -- visited) map (_ :: path)
              step(rest ++ nextPaths, visited + on)
          }
        }
      }
    }
    step(Vector(List(from)), Set.empty)
  }

  def airNeighbors(pos: Pos) = pos.neighbors filter isAir
}
