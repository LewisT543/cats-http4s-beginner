import CatsTCOverview.Functor

object FunctorExample {

  // Upon creating do10xGeneral, we are able to use it instead of the more specific implementations below
  // - we do this using an implicit / using clause with the available Functor
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)

  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)

  //  import cats.syntax.functor._
  def do10xGeneral[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    functor.map(container)(_ * 10)
}
