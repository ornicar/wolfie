package vindinium.wolfie

import Dir._
import Tile._

class Wolfie {

  val beerGold = -2
  val dayLife = -1
  val mineLife = -20
  val defendLife = -20

  def move(input: Input) = {

    import input._

    val teamPlay: Boolean = game.heroes.map(_.name).distinct.size > 1
    val heroIds: Set[Int] = input.game.heroes map (_.id) toSet
    val enemies: List[Hero] =
      if (teamPlay) game.heroes filter (_.name != hero.name)
      else game.heroes filter (_.id != hero.id)
    val enemyIds: Set[Int] = enemies.map(_.id).toSet

    def isTavern(pos: Pos) = is(pos, Tile.Tavern==)
    def isNewMine(pos: Pos) = is(pos, {
      case Tile.Mine(h) => h.fold(true)(enemyIds)
      case _            => false
    })
    def isEnemy(id: Int)(pos: Pos) = is(pos, Tile.Hero(id)==)
    def isAir(pos: Pos) = is(pos, Tile.Air==)
    def is(pos: Pos, f: Tile => Boolean) = game.board at pos exists f

    // BFS: shortest path to a tile satisfying goal function
    def ~>(from: Pos, goal: Pos => Boolean): Path = {
      @annotation.tailrec
      def step(paths: Vector[Path], visited: Set[Pos]): Path = paths match {
        case Vector() => Nil
        case path +: rest =>
          val on = path.head
          if (visited(on)) step(rest, visited) else {
            on.neighbors find goal match {
              case Some(found) => (found :: path).reverse drop 1
              case None        => step(rest ++ (airNeighbors(on) -- visited).map(_ :: path), visited + on)
            }
          }
      }
      step(Vector(List(from)), Set.empty)
    }

    def airNeighbors(pos: Pos) = pos.neighbors filter isAir

    val tavernPath = ~>(hero.pos, isTavern)
    val minePath = ~>(hero.pos, isNewMine)
    val enemyPaths = enemies map { h => (h, ~>(hero.pos, isEnemy(h.id))) }

    def aybabtu = minePath.isEmpty

    def goHunt = enemyPaths collectFirst {
      case (enemy, path) if attackable(enemy, path.size) => path
    }
    def goMine = if (hero.life + mineLife + minePath.size * dayLife > 5) Some(minePath) else None

    def attackable(enemy: Hero, distance: Int) = {
      def wonFight = distance < 3 && enemy.life < hero.life
      def safePrey = enemy.mineCount > 0 &&
        (Set(1, 2, 4, 5) contains distance) &&
        (enemy.life + defendLife) < hero.life
      def wealthyPrey = enemy.mineCount > 1 &&
        distance < 6 &&
        (enemy.life + defendLife) < hero.life
      safePrey || wealthyPrey
    }

    val path =
      if (tavernPath.size == 1 && hero.life < 80 && hero.gold >= beerGold) tavernPath
      else if (aybabtu) if (tavernPath.size > 1) tavernPath else Nil
      else goHunt orElse goMine getOrElse tavernPath

    ~path.headOption.flatMap(hero.pos.dirTo)
  }
}
