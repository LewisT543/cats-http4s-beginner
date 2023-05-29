

object CatsTCOverview {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }
  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }
  /*
    Semigroup -> Monoid

    SemiGroupal -> Apply -> FlatMap --->
                 /       \              \
    Functor ---->         Applicative --> Monad --------> MonadError
                                     \
                                      ApplicativeError

  */

  // import cats.Functor == below impl
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  // ------------------------------ The Weird Ones (start) ------------------------------ //

  trait SemiGroupal[F[_]] {
    def /*cartesian*/ product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  trait Apply[F[_]] extends SemiGroupal[F] with Functor[F] {
    def ap[A, B](fab: F[A => B], fa: F[A]): F[B]

    def /*cartesian*/ product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
      val myFunction: A => B => (A, B) = (a: A) => (b: B) => (a, b)
      val fab: F[B => (A, B)] = map(fa)(myFunction)
      ap(fab, fb)
    }

    def mapN[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
      map(product(fa, fb)) {
        case (a, b) => f(a, b)
      }
    }
  }

  // ------------------------------- The Weird Ones (end) ------------------------------- //

  // Applicatives - lift type A => F[A]
  trait Applicative[F[_]] extends Apply[F] {
    def pure[A](a: A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] = {
      ap(pure(f), fa)
    }
  }

  // FlatMap describes anything that allows for chaining of computations
  trait FlatMap[F[_]] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  trait Monad[F[_]] extends Applicative[F] with FlatMap[F] {
    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(a => pure(f(a)))

    // Monad also has the capability to do iterative stuff (tailRecM in cats)
  }

  trait ApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](error: E): F[A]
  }

  trait MonadError[F[_], E] extends ApplicativeError[F, E] with Monad[F]


  def main(args: Array[String]): Unit = {

  }
}
