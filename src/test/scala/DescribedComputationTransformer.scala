import DescribedComputationTransformer.comp
import treelog.LogTreeSyntaxWithoutAnnotations
import treelog.LogTreeSyntaxWithoutAnnotations.{DescribedComputation, _}

import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz._

object DescribedComputationTransformer extends App {


  // トランスフォーマー本体
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
      // やりたいこと: selfとfからF[DescribedComputation[B]]を作る。
      val v: F[DescribedComputation[B]] = F.bind(self.run) { dcA =>
        // dcA.flatMapは使えない。なぜならfはF[DescribedComputation]を返すから。
        // dcAからなんどかAを取り出して直接fに入れられればいい。
        // 幸い、dcAはWriterなのでdcA.run.valueで最終結果を直接得られる！
        dcA.run.value match {
          case \/-(a) =>
            // 成功していたら
            for {
              dcB <- f(a).run
            } yield {
              // 過去の計算(dcA)の履歴だけくっつけて
              // 結果はbを返すDescribedComputation[B]にして返す
              for {
                _ <- dcA // ここでは履歴だけprependしたいので値は捨てる
                b <- dcB // aを元に計算されたDescribedComputation[B]
              } yield b  // 最終結果はb
            }
          case -\/(error) =>
            // 失敗していたらBを返すDescribedComputationに変換しておく
            val eitherWriter = EitherT.monadListen[LogTreeWriter, LogTree, String]

            def failure[V](description: String, tree: LogTree): DescribedComputation[V] =
              for {
                _ ← eitherWriter.tell(tree)
                err ← eitherWriter.left[V](description)
              } yield err

            F.point(failure[B](error, dcA.run.written))
        }
      }
      // 最後にtransformerに包んで完成
      DescribedComputationT(v)
    }
  }

  object DescribedComputationT {

    // トランスフォーマーはモナド
    private trait DescribedComputationTMonad[F[_]] extends Monad[DescribedComputationT[F, ?]] {
      implicit def F: Monad[F]

      override def bind[A, B](fa: DescribedComputationT[F, A])(f: (A) => DescribedComputationT[F, B]): DescribedComputationT[F, B]
      = fa.flatMap(f)

      override def point[A](a: => A): DescribedComputationT[F, A] = DescribedComputationT(F.pure(success(a)))
    }

    implicit def describedComputationTMonad[F[_]](implicit F0: Monad[F]): Monad[DescribedComputationT[F, ?]] = new DescribedComputationTMonad[F] {
      implicit def F: Monad[F] = F0
    }

    // トランスフォーマーの型クラスにしておくことでsyntaxが使えるようにもしておく
    private trait DescribedComputationTHoist extends Hoist[DescribedComputationT] {
      override def hoist[M[_] : Monad, N[_]](f: M ~> N)
      = λ[DescribedComputationT[M, ?] ~> DescribedComputationT[N, ?]](_ mapT f)

      def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): DescribedComputationT[G, A]
      = DescribedComputationT(G.map[A, DescribedComputation[A]](a)((a: A) => success(a)))

      implicit def apply[G[_] : Monad]: Monad[DescribedComputationT[G, ?]]
      = DescribedComputationT.describedComputationTMonad[G]
    }

    implicit val dcTrans: Hoist[DescribedComputationT] = new DescribedComputationTHoist {}
  }

  // モナドな F[V] という値の計算をdescribed computation化する為のメソッド群
  implicit class DescribedComputationTSyntax[F[_], A](fa: F[A])(implicit m: Monad[F]) {

    def ~>(description: String): DescribedComputationT[F, A] = ~>(_ => description)

    def ~>(description: A => String): DescribedComputationT[F, A] = DescribedComputationT(fa.map(v => v ~> description(v)))
  }

  // これがやりたい
  import scalaz._
  import effect.IO
  import IO._

  val comp = for {
    line <- readLn ~> ("readLn: " + _)
    res <- putStrLn(line) ~> ("put line read: " + _)
  } yield {
    res
  }

  println("perform unsafePerformIO")
  println("input some string: ")
  val writer =  comp.run.unsafePerformIO.run

  println("output described computation results")
  println(writer.written.shows)
  println(writer.value)

}
