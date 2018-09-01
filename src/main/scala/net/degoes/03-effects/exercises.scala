// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.effects

import net.degoes.effects.zio_background.Program.Return
import scalaz.zio.ExitResult.Completed
import scalaz.zio._
import scalaz.zio.console._

import scala.concurrent.duration._
import scala.language.higherKinds
import scala.util.Try

object zio_background {
  sealed trait Program[A] {
    self =>

    import Program.{Chain, Return}

    final def map[B](f: A => B): Program[B] =
      Chain(self, f.andThen(Return(_)))

    final def flatMap[B](f: A => Program[B]): Program[B] =
      Chain(self, f)
  }
  object Program {
    final case class ReadLine[A](next: String => Program[A]) extends Program[A]
    final case class WriteLine[A](line: String, next: Program[A]) extends Program[A]
    final case class Chain[A0, A](previous: Program[A0], next: A0 => Program[A]) extends Program[A]
    final case class Return[A](value: A) extends Program[A]

    def readLine: Program[String] = ReadLine(Return(_))
    def writeLine(line: String): Program[Unit] = WriteLine(line, Return(()))
    def point[A](a: A): Program[A] = Return(a)
  }

  import Program.{point, readLine, writeLine}

  val yourName1: Program[Unit] =
    writeLine("What is your name?").flatMap(_ =>
      readLine.flatMap(name =>
        writeLine("Hello, " + name + ", good to meet you!").flatMap(_ =>
          point(())
        )
      )
    )

  //
  // EXERCISE 1
  //
  // Rewrite `program1` to use a for comprehension.
  //
  val yourName2: Program[Unit] =
  for {
    _ <- writeLine("What is your name?")
    name <- readLine
    _ <- writeLine("Hello, " + name + ", good to meet you!")
  } yield ()

  //
  // EXERCISE 2
  //
  // Rewrite `yourName2` using the helper function `getName`, which shows how
  // to create larger programs from smaller programs.
  //
  def yourName3: Program[Unit] =
    getName.map(name => writeLine("Hello, " + name + ", good to meet you!"))

  def getName: Program[String] =
    writeLine("What is your name?").flatMap(_ => readLine)

  //
  // EXERCISE 3
  //
  // Implement the following effectful procedure, which interprets
  // `Program[A]` into `A`. You can use this procedure to "run" programs.
  //
  def interpret[A](program: Program[A]): A = program match {
    case Program.Return(value) => value
    case Program.WriteLine(line, next) => println(line); interpret(next)
    case Program.ReadLine(next) => interpret(next(scala.io.StdIn.readLine()))
    case Program.Chain(previous, next) => interpret(next(interpret(previous)))
  }

  //
  // EXERCISE 4
  //
  // Implement the following function, which shows how to write a combinator
  // that operates on programs.
  //
  def sequence[A](programs: List[Program[A]]): Program[List[A]] =
    programs match {
      case Nil => Program.point(Nil)
      case h :: t => h.flatMap(a => sequence(t).map(as => a :: as))
    }

  def sequence2[A](programs: List[Program[A]]): Program[List[A]] =
    programs.foldRight(Return(List.empty[A]): Program[List[A]])((e, acc) => {
      e.flatMap(a => acc.map(a :: _))
    })

  //
  // EXERCISE 5
  //
  // Translate the following procedural program into a purely functional program
  // using `Program` and a for comprehension.
  //
  def ageExplainer1(): Unit = {
    println("What is your age?")
    scala.util.Try(scala.io.StdIn.readLine().toInt).toOption match {
      case Some(age) =>
        if (age < 12) println("You are a kid")
        else if (age < 20) println("You are a teenager")
        else if (age < 30) println("You are a grownup")
        else if (age < 50) println("You are an adult")
        else if (age < 80) println("You are a mature adult")
        else if (age < 100) println("You are elderly")
        else println("You are probably lying.")
      case None =>
        println("That's not an age, try again")

        ageExplainer1()
    }

  }
  def ageExplainer2: Program[Unit] =
    for {
      _ <- writeLine("What is your age?")
      age <- readLine
      _ <-
        Try(age.toInt).toOption
          .map(age =>
            writeLine(
              if (age < 12) "You are a kid"
              else if (age < 20) "You are a teenager"
              else if (age < 30) "You are a grownup"
              else if (age < 50) "You are an adult"
              else if (age < 80) "You are a mature adult"
              else if (age < 100) "You are elderly"
              else "You are probably lying."
            )
          )
          .getOrElse(writeLine("That's not an age, try again").flatMap(_ => ageExplainer2))
    } yield ()
}

object zio_type {
  type ??? = Nothing

  //
  // EXERCISE 1
  //
  // Write the type of `IO` values that can fail with an `Exception`, or
  // may produce an `A`.
  //
  type Exceptional[A] = IO[Exception, A]

  //
  // EXERCISE 2
  //
  // Write the type of `IO` values that can fail with a `Throwable`, or
  // may produce an `A`.
  //
  type Task[A] = IO[Throwable, A]

  //
  // EXERCISE 3
  //
  // Write the type of `IO` values that cannot fail, but may produce an `A.`
  //
  type Infallible[A] = IO[Nothing, A]

  //
  // EXERCISE 4
  //
  // Write the type of `IO` values that cannot produce a value, but may fail
  // with an `E`.
  //
  type Unproductive[E] = IO[E, Nothing]

  //
  // EXERCISE 5
  //
  // Write the type of `IO` values that cannot fail or produce a value.
  //
  type Unending = IO[Nothing, Nothing]
}

object zio_values {
  //
  // EXERCISE 1
  //
  // Lift the integer `2` into a strictly-evaluated `IO`.
  //
  val ioInteger: IO[Nothing, Int] = IO.now(2)

  //
  // EXERCISE 2
  //
  // Lift the string "Functional Scala" into a lazily-evaluated `IO`.
  //
  val ioString: IO[Nothing, String] = IO.point("Functional Scala")

  //
  // EXERCISE 3
  //
  // Lift the string "Bad Input" into a failed `IO`.
  val failedInput: IO[String, Nothing] = IO.fail("Bad Input")
}

object zio_composition {
  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }
  //
  // EXERCISE 1
  //
  // Map the `IO[Nothing, Int]` into an `IO[Nothing, String]` by converting the
  // integer into its string rendering using the `map` method of `IO`.
  //
  IO.point(42).map(Integer.toString)

  //
  // EXERCISE 2
  //
  // Map the `IO[Int, Nothing]` into an `IO[String, Nothing]` by converting the
  // integer error into its string rendering using the `leftMap` method of `IO`.
  IO.fail(42).leftMap(Integer.toString)

  //
  // EXERCISE 3
  //
  // Using the `flatMap` method of `IO`, add `ioX` and `ioY` together.
  //
  val ioX: IO[Nothing, Int] = IO.point(42)
  val ioY: IO[Nothing, Int] = IO.point(58)
  val ioXPlusY: IO[Nothing, Int] = ioX.flatMap(x => ioY.map(_ + x))

  //
  // EXERCISE 4
  //
  // Using the `flatMap` method of `IO`, implement `ifThenElse`.
  //
  def ifThenElse[E, A](bool: IO[E, Boolean])(ifTrue: IO[E, A], ifFalse: IO[E, A]): IO[E, A] =
    bool.flatMap(b => if (b) ifTrue else ifFalse)
  val exampleIf = ifThenElse(IO.point(true))(IO.point("It's true!"), IO.point("It's false!"))

  //
  // EXERCISE 5
  //
  // Translate the following program, which uses for-comprehensions, to its
  // equivalent chain of `flatMap`'s, followed by a final `map`.
  //
  for {
    v1 <- IO.point(42)
    v2 <- IO.point(58)
    v3 <- IO.point("The Total Is: ")
  } yield v3 + (v1 + v2).toString

  IO.point(42).flatMap(v1 =>
    IO.point(58).flatMap(v2 =>
      IO.point("The Total Is: ").map(v3 => v3 + (v1 + v2))
    )
  )

  //
  // EXERCISE 6
  //
  // Rewrite the following ZIO program, which uses conditionals, into its
  // procedural equivalent.
  //
  def analyzeAverageAge1(spouse1: IO[Nothing, Int], spouse2: IO[Nothing, Int]): IO[Nothing, String] =
    for {
      age1 <- spouse1
      age2 <- spouse2
      rez <- if (((age1 + age2) / 2) < 40) IO.point("You don't have kids yet")
      else IO.point("You might have kids yet")
    } yield rez
  def analyzeAverageAge2(spouse1: Int, spouse2: Int): String =
    if (((spouse1 + spouse2) / 2) < 40) "You don't have kids yet"
    else "You might have kids yet"

  //
  // EXERCISE 7
  //
  // Rewrite the following procedural program, which uses conditionals, into its
  // ZIO equivalent.
  //
  def analyzeName1(first: String, last: String): String =
    if ((first + " " + last).length > 20) "Your full name is really long"
    else if ((first + last).contains(" ")) "Your name is really weird"
    else "Your name is pretty normal"
  def analyzeName2(first: IO[Nothing, String], last: IO[Nothing, String]): IO[Nothing, String] =
    for {
      f <- first
      l <- last
    } yield
      if ((f + l).length > 20) "Your full name is really long"
      else if ((f + l).contains(" ")) "Your name is really weird"
      else "Your name is pretty normal"

  //
  // EXERCISE 8
  //
  // Translate the following loop into its ZIO equivalent.
  //
  def sumList1(ints: List[Int], acc: Int): Int = ints match {
    case Nil => acc
    case x :: xs => sumList1(xs, acc + x)
  }
  def sumList2(ints: IO[Nothing, List[Int]], acc: IO[Nothing, Int]): IO[Nothing, Int] =
    ints.map(_.sum)

  //
  // EXERCISE 9
  //
  // Translate the following loop into its ZIO equivalent.
  //
  def decrementUntilFour1(int: Int): Unit =
    if (int < 4) ()
    else if ((int / 2 + 2) == int) () else decrementUntilFour1(int - 1)
  def decrementUntilFour2(int: IO[Nothing, Int]): IO[Nothing, Unit] =
    int.map(i =>
      if (i < 4) ()
      else if ((i / 2 + 2) == i) () else decrementUntilFour1(i - 1)
    )

  //
  // EXERCISE 10
  //
  // Translate the following expression into its `flatMap` equivalent.
  //
  IO.point(42) *> IO.point(19)
  IO.point(42).flatMap(_ => IO.point(19))

  //
  // EXERCISE 11
  //
  // Translate the following expression into its `flatMap` equivalent.
  //
  IO.point(42) <* IO.point(19)
  IO.point(42).flatMap(lhs => IO.point(19).map(_ => lhs))

  //  import cats.implicits._
  //  def foo[M[_]: Monad, A](a: M[A]) = ???
  //  foo(Either.right(5))
  //  (Option(5), Option(5)).mapN(_ + _)

  //
  // EXERCISE 12
  //
  // Translate the following expression into its `flatMap` equivalent.
  //
  (IO.point(42) <* IO.point(19)) *> IO.point(1)
  IO.point(42).flatMap(lhs => IO.point(19).map(_ => lhs)).flatMap(_ => IO.point(1))
  for {
    _ <- IO.point(42)
    _ <- IO.point(19)
    r <- IO.point(1)
  } yield r
}

object zio_failure {
  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  //
  // EXERCISE 1
  //
  // Create an `IO[String, Int]` value that represents a failure with a string
  // error message, containing a user-readable description of the failure.
  val stringFailure1: IO[String, Int] = IO.fail("File could not be opened")

  //
  // EXERCISE 2
  //
  // Create an `IO[Int, String]` value that represents a failure with an integer
  // error code.
  //
  val intFailure: IO[Int, String] = IO.fail(42)

  //
  // EXERCISE 3
  //
  // Transform the error of `intFailure` into its string representation using
  // the `leftMap` method of `IO`.
  //
  val stringFailure2: IO[String, String] = intFailure.leftMap(Integer.toString)

  //
  // EXERCISE 4
  //
  // Translate the following exception-throwing program into its ZIO equivalent.
  //
  def accessArr1[A](i: Int, a: Array[A]): A =
    if (i < 0 || i >= a.length) throw new IndexOutOfBoundsException("The index " + i + " is out of bounds [0, " + a.length + ")")
    else a(i)
  def accessArr2[A](i: Int, a: Array[A]): IO[IndexOutOfBoundsException, A] =
    if (i < 0 || i >= a.length) IO.fail(new IndexOutOfBoundsException("The index " + i + " is out of bounds [0, " + a.length + ")"))
    else IO.now(a(i))

  //
  // EXERCISE 5
  //
  // Translate the following ZIO program into its exception-throwing equivalent.
  //
  trait DenomIsZero
  object DenomIsZero extends DenomIsZero {}
  def divide1(n: Int, d: Int): IO[DenomIsZero, Int] =
    if (d == 0) IO.fail(DenomIsZero)
    else IO.now(n / d)
  def divide2(n: Int, d: Int): Int =
    if (d == 0) throw new IllegalArgumentException("d is 0")
    else n / d

  //
  // EXERCISE 6
  //
  // Recover from a division by zero error by returning `-1`.
  //
  val recovered1: IO[Nothing, Int] = divide1(100, 0).attempt.map {
    case Left(_) => -1
    case Right(value) => value
  }

  //
  // EXERCISE 7
  //
  // Recover from a dvision by zero error by using `redeem`.
  //
  val recovered2: IO[Nothing, Int] = divide1(100, 0).redeem(_ => IO.now(-1), IO.now)
  val recovered2bis: IO[Nothing, Int] = divide1(100, 0).catchAll(_ => IO.now(-1))

  //
  // EXERCISE 8
  //
  // Use the `orElse` method of `IO` to try `firstChoice`, and fallback to
  // `secondChoice` only if `firstChoice` fails.
  //
  val firstChoice: IO[DenomIsZero, Int] = divide1(100, 0)
  val secondChoice: IO[Nothing, Int] = IO.now(400)
  val combined: IO[Nothing, Int] = firstChoice.orElse(secondChoice)
}

object zio_effects {

  import java.io.File
  import java.util.concurrent.{Executors, TimeUnit}

  import scala.io.Source

  type ??? = Nothing
  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  //
  // EXERCISE 1
  //
  // Using the `IO.sync` method, wrap Scala's `println` method to import it into
  // the world of pure functional programming.
  //
  def putStrLn(line: String): IO[Nothing, Unit] = IO.sync(println(line))

  //
  // EXERCISE 2
  //
  // Using the `IO.sync` method, wrap Scala's `readLine` method to import it
  // into the world of pure functional programming.
  //
  val getStrLn: IO[Nothing, String] = IO.sync(scala.io.StdIn.readLine())

  //
  // EXERCISE 3
  //
  // Using the `IO.syncException` method, wrap Scala's `getLines` method to
  // import it into the world of pure functional programming.
  //
  def readFile(file: File): IO[Exception, List[String]] =
    IO.syncException(Source.fromFile(file).getLines.toList)

  //
  // EXERCISE 4
  //
  // Identify the correct method and error type to import `System.nanoTime`
  // safely into the world of pure functional programming.
  //
  def nanoTime: IO[Nothing, Long] = IO.sync(System.nanoTime())

  //
  // EXERCISE 5
  //
  // Identify the correct method, error, and value type to import `System.exit`
  // safely into the world of pure functional programming.
  //
  def sysExit(code: Int): IO[SecurityException, Unit] = IO.syncCatch(System.exit(code)) {
    case t: SecurityException => t
  }

  //
  // EXERCISE 6
  //
  // Identify the correct method, error, and value type to import
  // `Array.update` safely into the world of pure functional programming.
  //
  def arrayUpdate[A](a: Array[A], i: Int, f: A => A): IO[IndexOutOfBoundsException, Unit] =
    IO.syncCatch(a.update(i, f(a(i)))) {
      case t: IndexOutOfBoundsException => t
    }

  //
  // EXERCISE 7
  //
  // Use the `IO.async` method to implement the following `sleep` method, and
  // choose the correct error type.
  //
  val scheduledExecutor = Executors.newScheduledThreadPool(1)
  def sleep(l: Long, u: TimeUnit): IO[Nothing, Unit] =
    IO.async((k: Callback[Nothing, Unit]) => scheduledExecutor.schedule(new Runnable {
      def run(): Unit = k(Completed(()))
    }, l, u))

  //
  // EXERCISE 8
  //
  // Translate the following procedural program into ZIO.
  //
  def playGame1(): Unit = {
    val number = scala.util.Random.nextInt(5)
    println("Enter a number between 0 - 5: ")
    scala.util.Try(scala.io.StdIn.readLine().toInt).toOption match {
      case None =>
        println("You didn't enter an integer!")
        playGame1
      case Some(guess) if (guess == number) =>
        println("You guessed right! The number was " + number)
      case _ =>
        println("You guessed wrong! The number was " + number)
    }
  }
  def playGame2: IO[Exception, Unit] = ???
}

object zio_concurrency {
  type ??? = Nothing

  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  //
  // EXERCISE 1
  //
  // Race `leftContestent1` and `rightContestent1` using the `race` method of
  // `IO` to see which one finishes first with a successful value.
  //
  val leftContestent1 = IO.never
  val rightContestent1 = putStrLn("Hello World")
  val raced1 = leftContestent1.race(rightContestent1)

  //
  // EXERCISE 2
  //
  // Race `leftContestent2` and `rightContestent2` using the `race` method of
  // `IO` to see which one finishes first with a successful value.
  //
  val leftContestent2: IO[Exception, Nothing] = IO.fail(new Exception("Uh oh!"))
  val rightContestent2: IO[Exception, Unit] = IO.sleep(10.milliseconds) *> putStrLn("Hello World")
  val raced2: IO[Exception, Unit] = leftContestent1.race(rightContestent2)

  //
  // EXERCISE 3
  //
  // Compute `leftWork1` and `rightWork1` in parallel using the `par` method of
  // `IO`.
  //
  val leftWork1: IO[Nothing, Int] = fibonacci(10)
  val rightWork1: IO[Nothing, Int] = fibonacci(10)
  val par1: IO[Nothing, (Int, Int)] = leftWork1.par(rightWork1)
  val par2 = leftWork1.attempt.par(rightWork1)

  //
  // EXERCISE 4
  //
  // Compute all values `workers` in parallel using `IO.parAll`.
  //
  val workers: List[IO[Nothing, Int]] = (1 to 10).toList.map(fibonacci(_))
  val workersInParallel: IO[Nothing, List[Int]] = IO.parAll(workers)

  //
  // EXERCISE 5
  //
  // Implement `myPar` by forking `left` and `right`, and then joining them
  // and yielding a tuple of their results.
  //
  def myPar[E, A, B](left: IO[E, A], right: IO[E, B]): IO[E, (A, B)] =
    for {
      fiberL <- left.fork
      fiberR <- right.fork
      l <- fiberL.join
      r <- fiberR.join
    } yield (l, r)

  //
  // EXERCISE 6
  //
  // Use the `IO.supervise` method to ensure that when the main fiber exits,
  // all fibers forked within it will be terminated cleanly.
  //
  val supervisedExample: IO[Nothing, Unit] =
  (for {
    fiber <- fibonacci(10000).fork
  } yield ()) ?

  //
  // EXERCISE 7
  //
  // Use the `interrupt` method of the `Fiber` object to cancel the long-running
  // `fiber`.
  //
  val interrupted1: IO[Nothing, Unit] =
  for {
    fiber <- fibonacci(10000).fork
  } yield ()

  //
  // EXERCISE 8
  //
  // Use the `zipWith` method of the `Fiber` object to combine `fiber1` and
  // `fiber2` into a single fiber (by summing the results), so they can be
  // interrupted together.
  //
  val interrupted2: IO[Nothing, Unit] =
  for {
    fiber1 <- fibonacci(10).fork
    fiber2 <- fibonacci(20).fork
    both <- (??? : IO[Nothing, Fiber[Nothing, Int]])
    _ <- both.interrupt
  } yield ()

  def fibonacci(n: Int): IO[Nothing, Int] =
    if (n <= 1) IO.now(n)
    else fibonacci(n - 1).seqWith(fibonacci(n - 2))(_ + _)
}

object zio_resources {

  import java.io.{File, FileInputStream}

  class InputStream private(is: FileInputStream) {
    def read: IO[Exception, Option[Byte]] =
      IO.syncException(is.read).map(i => if (i < 0) None else Some(i.toByte))
    def close: IO[Exception, Unit] =
      IO.syncException(is.close())
  }
  object InputStream {
    def openFile(file: File): IO[Exception, InputStream] =
      IO.syncException(new InputStream(new FileInputStream(file)))
  }

  //
  // EXERCISE 1
  //
  // Rewrite the following procedural program to ZIO, using `IO.fail` and the
  // `bracket` method of the `IO` object.
  //
  def tryCatch1(): Unit =
    try throw new Exception("Uh oh")
    finally println("On the way out...")
  val tryCatch2: IO[Exception, Unit] =
    ???

  //
  // EXERCISE 2
  //
  // Rewrite the `readFile1` function to use `bracket` so resources can be
  // safely cleaned up in the event of errors, defects, or interruption.
  //
  def readFile1(file: File): IO[Exception, List[Byte]] = {
    def readAll(is: InputStream, acc: List[Byte]): IO[Exception, List[Byte]] =
      is.read.flatMap {
        case None => IO.now(acc.reverse)
        case Some(byte) => readAll(is, byte :: acc)
      }

    for {
      stream <- InputStream.openFile(file)
      bytes <- readAll(stream, Nil)
    } yield bytes
  }
  def readFile2(file: File): IO[Exception, List[Byte]] = ???

  //
  // EXERCISE 3
  //
  // Implement the `tryCatchFinally` method using `bracket`.
  //
  def tryCatchFinally[E, A]
  (try0: IO[E, A])
  (catch0: PartialFunction[E, IO[E, A]])
  (finally0: IO[Nothing, Unit]): IO[E, A] = ???

  //
  // EXERCISE 4
  //
  // Use the `tryCatchFinally` method to rewrite the following snippet to ZIO.
  //
  def readFileTCF1(file: File): List[Byte] = {
    var fis: FileInputStream = null

    try {
      fis = new FileInputStream(file)
      val array = Array.ofDim[Byte](file.length.toInt)
      fis.read(array)
      array.toList
    } catch {
      case e: java.io.IOException => Nil
    } finally if (fis != null) fis.close()
  }
  def readFileTCF2(file: File): IO[Exception, List[Byte]] =
    ???
}

object zio_schedule {
  implicit class FixMe[A](a: A) {
    def ? = ???
  }

  //
  // EXERCISE 1
  //
  // Using `Schedule.recurs`, create a schedule that recurs 5 times.
  //
  val fiveTimes: Schedule[Any, Int] = ???

  //
  // EXERCISE 2
  //
  // Using the `repeat` method of the `IO` object, repeat printing "Hello World"
  // five times to the console.
  //
  val repeated1 = putStrLn("Hello World") ?

  //
  // EXERCISE 3
  //
  // Using `Schedule.spaced`, create a schedule that recurs forever every 1
  // second.
  //
  val everySecond: Schedule[Any, Int] = ???

  //
  // EXERCISE 4
  //
  // Using the `&&` method of the `Schedule` object, the `fiveTimes` schedule,
  // and the `everySecond` schedule, create a schedule that repeats fives times,
  // every second.
  //
  val fiveTimesEverySecond = ???

  //
  // EXERCISE 5
  //
  // Using the `repeat` method of the `IO` object, repeat the action
  // putStrLn("Hi hi") using `fiveTimesEverySecond`.
  //
  val repeated2 = ???

  //
  // EXERCISE 6
  //
  // Using the `andThen` method of the `Schedule` object, the `fiveTimes`
  // schedule, and the `everySecond` schedule, create a schedule that repeats
  // fives times rapidly, and then repeats every second forever.
  //
  val fiveTimesThenEverySecond = ???

  //
  // EXERCISE 7
  //
  // Using the `retry` method of the `IO` object, retry the following error
  // a total of five times.
  //
  val error1 = IO.fail("Uh oh!")
  val retried5 = error1 ?
}

object zio_interop {
  // import scalaz.zio.interop._
  // import scalaz.zio.interop.catz._
}

object zio_ref {

}

object zio_promise {

}

object zio_queue {

}

object zio_rts {

}

object zio_challenge {

}

object zio_advanced {

}
