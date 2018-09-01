package net.degoes

import scalaz.zio._
import scalaz.zio.console._

object effect_as_values {
  sealed trait Program[+A] {self =>
    def map[B](f: A => B): Program[B] = flatMap(a => Return(f(a)))

    def flatMap[B](f: A => Program[B]): Program[B] = self match {
      case Return(a) => f(a)
      case _ => Chain(self, f)
    }
  }

  object Program {
    def point[A](a: A) = Return(a)
  }

  case class Return[A](a: A) extends Program[A]
  case class GetStrLn[A](next: String => Program[A]) extends Program[A]
  case class PutStrLn[A](line: String, next: Program[A]) extends Program[A]
  case class Chain[A0, A](previous: Program[A0], next: A0 => Program[A]) extends Program[A]

  val getStrLn: Program[String] = GetStrLn(Program.point[String])
  def putStrLn(line: String): Program[Unit] = PutStrLn(line, Return(()))

  for {
    _ <- putStrLn("Welcome ...")
    name <- getStrLn
    _ <- putStrLn(s"Hello, $name")
  } yield ()
}

object FunctionalScala extends App {
  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
      _ <- putStrLn("Hello World!")
    } yield
      ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))
}
