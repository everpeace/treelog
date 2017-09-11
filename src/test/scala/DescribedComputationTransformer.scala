import org.scalacheck.{Arbitrary, Properties}
import treelog.LogTreeSyntaxWithoutAnnotations.{DescribedComputation, _}

import scala.language.higherKinds
import scalaz._
import Scalaz._
import scalaz.scalacheck.ScalazProperties._

class DescribedComputationTransformer extends Properties("") {

  def checkAll(props: Properties) {
    for ((name, prop) <- props.properties) yield {
      property(name) = prop
    }
  }

  // Transformer definition
  case class DescribedComputationT[F[_], A](run: F[DescribedComputation[A]]) {
    self =>

    def map[B](f: A => B)(implicit F: Functor[F]): DescribedComputationT[F, B]
    = {
      val v: F[DescribedComputation[B]] = F.map(self.run)(_ map f)
      DescribedComputationT(v)
    }

    def mapT[G[_], B](f: F[DescribedComputation[A]] => G[DescribedComputation[B]]): DescribedComputationT[G, B]
    = {
      val v: G[DescribedComputation[B]] = f(self.run)
      DescribedComputationT(v)
    }

    def flatMap[B](f: A => DescribedComputationT[F, B])(implicit F: Monad[F]): DescribedComputationT[F, B]
    = {
      val v: F[DescribedComputation[B]] = F.bind(self.run) { dcA =>
        // we can extract computation result from dcA because Writer contains "Id[?]".
        dcA.run.value match {
          case \/-(a) =>
            // if dcA succeeded,
            for {
              dcB <- f(a).run  // compute another described computation returning B
            } yield {
              // we can concatenate computation history like this.
              for {
                _ <- dcA // we can drop returned value because we just need to concatenate descriptions.
                b <- dcB // extracting final result of type B
              } yield b  // final result of this described computation should be B
            }
          case -\/(_) =>
            // if previous computation failed, we just return the history.
            // but we need to convert type of return value into B.
            run.map(_.rightMap(_.asInstanceOf[B]))
        }
      }
      // need to wrap DescribedComputation finally
      DescribedComputationT(v)
    }
  }

  object DescribedComputationT {

    // Transformer should be Monad.
    private trait DescribedComputationTMonad[F[_]] extends Monad[DescribedComputationT[F, ?]] {
      implicit def F: Monad[F]

      override def bind[A, B](fa: DescribedComputationT[F, A])(f: (A) => DescribedComputationT[F, B]): DescribedComputationT[F, B]
      = fa.flatMap(f)

      override def point[A](a: => A): DescribedComputationT[F, A] = DescribedComputationT(F.pure(success(a)))
    }

    implicit def describedComputationTMonad[F[_]](implicit F0: Monad[F]): Monad[DescribedComputationT[F, ?]] = new DescribedComputationTMonad[F] {
      implicit def F: Monad[F] = F0
    }

    // Transformer instance
    private trait DescribedComputationTHoist extends Hoist[DescribedComputationT] {
      override def hoist[M[_] : Monad, N[_]](f: M ~> N)
      = λ[DescribedComputationT[M, ?] ~> DescribedComputationT[N, ?]](_ mapT f)

      def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): DescribedComputationT[G, A]
      = DescribedComputationT(G.map[A, DescribedComputation[A]](a)((a: A) => success(a)))

      implicit def apply[G[_] : Monad]: Monad[DescribedComputationT[G, ?]]
      = DescribedComputationT.describedComputationTMonad[G]
    }

    implicit val dcTrans: Hoist[DescribedComputationT] = new DescribedComputationTHoist {}

    implicit def describedComputationTEqual[F[_]: Monad, A](implicit F0: Equal[F[DescribedComputation[A]]]): Equal[DescribedComputationT[F, A]] =
      F0.contramap((_: DescribedComputationT[F, A]).run)
  }

  // helper methods to construct DescribedComputationT for monadic f values.
  implicit class DescribedComputationTSyntax[F[_], A](fa: F[A])(implicit m: Monad[F]) {

    def ~>(description: String): DescribedComputationT[F, A] = ~>(_ => description)

    def ~>(description: A => String): DescribedComputationT[F, A] = DescribedComputationT(fa.map(v => v ~> description(v)))
  }

  // checking laws
  import scalaz.scalacheck.ScalazArbitrary._
  def newProperties(name: String)(f: Properties => Unit): Properties = {
    val p = new Properties(name)
    f(p)
    p
  }
  // required arbitraries
  implicit def describedComputationTArbitrary[F[_]: Monad, A](implicit a: Arbitrary[F[A]]):Arbitrary[DescribedComputationT[F, A]]
  = Arbitrary( for( r <- a.arbitrary ) yield r ~> "test descritpion" )

  checkAll(newProperties("DescribedComputationT[Option,?]") { p =>
    p.include(functor.laws[DescribedComputationT[Option,?]])
    p.include(monad.laws[DescribedComputationT[Option,?]])
    p.include(monadTrans.laws[DescribedComputationT, Option])
  })

  checkAll(newProperties("DescribedComputationT[Maybe,?]") { p =>
    p.include(functor.laws[DescribedComputationT[Maybe,?]])
    p.include(monad.laws[DescribedComputationT[Maybe,?]])
    p.include(monadTrans.laws[DescribedComputationT, Maybe])
  })

  checkAll(newProperties("DescribedComputationT[List,?]") { p =>
    p.include(functor.laws[DescribedComputationT[List,?]])
    p.include(monad.laws[DescribedComputationT[List,?]])
    p.include(monadTrans.laws[DescribedComputationT, List])
  })

  checkAll(newProperties("DescribedComputationT[Tree,?]") { p =>
    p.include(functor.laws[DescribedComputationT[Tree,?]])
    p.include(monad.laws[DescribedComputationT[Tree,?]])
    p.include(monadTrans.laws[DescribedComputationT, Tree])
  })

// This is what we want to achieve
//  import effect.IO
//  import IO._
//
//  val comp = for {
//    line <- readLn ~> ("readLn: " + _)
//    res <- putStrLn(line) ~> ("put line read: " + _)
//  } yield {
//    res
//  }
//
//  println("perform unsafePerformIO")
//  println("input some string: ")
//  val writer =  comp.run.unsafePerformIO.run
//
//  println("output described computation results")
//  println(writer.written.shows)
//  println(writer.value)

}
