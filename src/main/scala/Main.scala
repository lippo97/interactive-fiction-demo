import Game.Event
import RoomDSL.Item

trait Direction {
  def name: String
}

object Direction {
  case object North extends Direction {
    override def name: String = "north"
  }
  case object South extends Direction {
    override def name: String = "south"
  }
  case object East extends Direction {
    override def name: String = "east"
  }
  case object West extends Direction {
    override def name: String = "west"
  }
  case object Up extends Direction {
    override def name: String = "up"
  }
  case object Down extends Direction {
    override def name: String = "down"
  }

  val all = Seq(North, South, East, West, Up, Down)
}

case class Path(direction: Direction, room: Room) {
  def describe: String = s"to the ${direction.name} there's ${room.outsideDescription}"
}

trait Room {
  var neighbors: Seq[Path]
  def outsideDescription: String
  def insideDescription: String
  def inspection: String
  var content: Seq[Item]
  def next(direction: Direction): Option[Room] =
    neighbors find { _.direction == direction } map { _.room }
  def describePaths: String = {
    if (this.neighbors.isEmpty) ""
    else
      this.neighbors
        .map(_.describe )
        .reduce(_ + ", " + _)
        .capitalize
  }
}

object Room {
  def apply(
    outsideDescription: String,
    insideDescription: String,
    neighbors: Seq[Path] = Seq(),
    content: Seq[Item] = Seq()
  ): Room = new RoomImpl(outsideDescription, insideDescription, neighbors, content)

  class RoomImpl(
    override val outsideDescription: String,
    override val insideDescription: String,
    override var neighbors: Seq[Path],
    override var content: Seq[Item]
  ) extends Room {
    override def inspection: String = {
      val contentDescription = if (content.isEmpty) "" else content.map(_.inspect.capitalize).reduce(_+"\n"+_)
      insideDescription
        .concat(".\n")
        .concat(describePaths)
        .concat(".\n")
        .concat(contentDescription)
    }
  }
}

object RoomDSL {

  def isOrAre(quantity: Int): String = if (quantity == 1) "is" else "are"

  implicit class EnhancedRoomBuilder(room: Room) {
    def <--> (other: Room): Room = {
      room.neighbors = room.neighbors :+ Path(Direction.East, other)
      other.neighbors = other.neighbors :+ Path(Direction.West, room)
      other
    }
  }

  case class Item(
    name: String,
    description: String,
    quantity: Int = 0,
    var inside: Option[Seq[Item]] = None,
    var on: Option[Seq[Item]] = None
  ) {

    def inspect: String = {
      val descriptionOnTop = on map {
        case Nil => "There's nothing on it."
        case items => s"On top of it there ${isOrAre(items.map(_.quantity).sum)} "
          .concat(items.map(_.description).reduce(_+", "+_).concat("."))
      } map { "\n" + _ }

      s"There ${isOrAre(quantity)} ${description}."
        .concat(descriptionOnTop.getOrElse(""))

    }
  }

  trait ABuilder {
    def a (item: Item): Unit
    def an (item: Item): Unit = this a item
  }

  class ABuilderInRoom(room: Room) extends ABuilder with InRoomLogic {
    def a (item: Item): Unit = putIn(room)(item)
  }

  class ABuilderInItem(item: Item) extends ABuilder with InItemLogic {
    def a (content: Item): Unit = putIn(item)(content)
  }

  class ABuilderOnItem(item: Item) extends ABuilder with OnItemLogic {
    def a (content: Item): Unit = putOn(item)(content)
  }

  trait InRoomLogic {
    def putIn(room: Room)(item: Item): Unit =
      room.content = room.content :+ item
  }

  trait OnItemLogic {
    def putOn(item: Item)(content: Item): Unit =
      item.on = item.on match {
        case None => Some(Seq(content))
        case x => x map { _ :+ content}
      }
  }

  trait InItemLogic {
    def putIn(item: Item)(content: Item): Unit =
      item.inside = item.inside match {
        case None => Some(Seq(content))
        case x => x map { _ :+ content }
      }
  }

  trait SomeBuilder {
    def some(item: Item): Unit
  }

  class SomeBuilderInItem(item: Item) extends SomeBuilder with InItemLogic {
    def some (content: Item): Unit = putIn(item)(content)
  }

  class SomeBuilderOnItem(item: Item) extends SomeBuilder with OnItemLogic {
    def some (content: Item): Unit = putOn(item)(content)
  }

  class SomeBuilderInRoom(room: Room) extends SomeBuilder with InRoomLogic {
    def some (content: Item): Unit = putIn(room)(content)
  }

  trait IsBuilder
  object IsBuilderImpl extends IsBuilder
  val is = IsBuilderImpl

  trait AreBuilder
  object AreBuilderImpl extends AreBuilder
  val are = AreBuilderImpl

  trait ThereBuilder {
    def there (is: IsBuilder): ABuilder
    def there (are: AreBuilder): SomeBuilder
  }

  class ThereBuilderRoom(room: Room) extends ThereBuilder {
    override def there (is: IsBuilder): ABuilder = new ABuilderInRoom(room)

    override def there(are: AreBuilder): SomeBuilder = new SomeBuilderInRoom(room)
  }

  class ThereBuilderOnItem(item: Item) extends ThereBuilder {
    override def there(is: IsBuilder): ABuilder = new ABuilderOnItem(item)

    override def there(are: AreBuilder): SomeBuilder = new SomeBuilderOnItem(item)
  }

  class ThereBuilderInItem(item: Item) extends ThereBuilder {
    override def there(is: IsBuilder): ABuilder = new ABuilderInItem(item)

    override def there(are: AreBuilder): SomeBuilder = new SomeBuilderInItem(item)
  }

  object InBuilder {
    def the (room: Room): ThereBuilder = new ThereBuilderRoom(room)

    def the (item: Item): ThereBuilder = new ThereBuilderInItem(item)
  }
  val in = InBuilder

  object OnBuilder {
    def the (item: Item): ThereBuilder = new ThereBuilderOnItem(item)
  }
  val on = OnBuilder

}

trait Action

case object Unknown extends Action

case class Move(direction: Direction) extends Action

case class Inspect(name: String) extends Action

object Game {
  def apply(room: Room): Game = new Game(room)

  trait Event

  object Event {
    case class InspectionResult(msg: String) extends Event
    case object ChangeRoom extends Event
    case object None extends Event
  }
}

class Game(private var room: Room) {

  def inspect: String = room.inspection

  def move(direction: Direction): Either[String, Event] = room next direction match {
    case Some(newRoom) =>
      room = newRoom
      Right(Event.ChangeRoom)
    case None => Left("There is no such direction.")
  }

  def inspect(name: String): Either[String, Event] =
    (room.content find (_.name.contains(name)))
      .map(i => Event.InspectionResult(s"It's ${i.description}."))
      .toRight("There is no such item.")

  def run(action: Action): Either[String, Event] = Right(action) flatMap {
    case Inspect(something) => inspect(something)
    case Move(direction) => move(direction)
    case _ => Left("There is no such action.")
  }
}

object Main extends App {

  import RoomDSL._

  val kitchen = Room("the kitchen", "You are in the kitchen of a house")
  val livingRoom = Room("the living room", "You are in the living room of a house")
  val diningRoom = Room("the dining room","You are in the dining room of a house")

  val table = Item("table", "a table", 1, on = Some(Seq()))
  val coin = Item("coin", "an old coin", 1)
  val fridge = Item("fridge", "a tall fridge", 1)
  val oranges = Item("orange", "some tasty oranges", 3)
  val knife = Item("knife", "a rusty knife", 1)
  val tv = Item("tv", "an old tv", 1)
  val emptyBottle = Item("bottle", "an empty bottle", 1)

  diningRoom <--> livingRoom <--> kitchen

  in the livingRoom there is a tv
  in the livingRoom there is a table
  in the kitchen there is a fridge
  in the fridge there are some oranges
  on the table there is a knife
  on the table there is an emptyBottle

  on the table there is a coin

  val game = new Game(livingRoom)
  var res: Either[String, String] = Right("")

  import cats.effect._
  import cats.implicits._
  def putStr(value: String) = IO(print(value))
  def putStrLn(value: String) = IO(println(value))
  val readLn = IO(scala.io.StdIn.readLine())

  putStrLn(game.inspect).unsafeRunSync()
  while(true) {
    val res = for {
      _ <- putStr("$ ")
      i <- readLn
      a <- IO.pure(i match {
        case "north" => Move(Direction.North)
        case "south" => Move(Direction.South)
        case "west" => Move(Direction.West)
        case "east" => Move(Direction.East)
        case "up" => Move(Direction.Up)
        case "down" => Move(Direction.Down)
        case s"inspect ${something}" => Inspect(something)
        case _ => Unknown
      })
      r <- IO.pure(game.run(a))
      _ <- r match {
        case Left(err) => putStrLn(err)
        case Right(event) => event match {
          case Event.ChangeRoom => putStrLn(game.inspect)
          case Game.Event.InspectionResult(msg) => putStrLn(msg)
          case Game.Event.None => IO.unit
          case _ => IO.unit
        }

      }
    } yield ()

    res.unsafeRunSync()
  }
}
