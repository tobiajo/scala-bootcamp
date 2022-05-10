package com.evolutiongaming.bootcamp.effects

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/*
 * Homework 1. Provide your own implementation of a subset of `IO` functionality.
 *
 * Provide also tests for this functionality in EffectsHomework1Spec (which you should create).
 *
 * Refer to:
 *  - https://typelevel.org/cats-effect/datatypes/io.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO$.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO.html
 * about the meaning of each method as needed.
 *
 * There are two main ways how to implement IO:
 * - Executable encoding  - express every constructor and operator for our model in terms of its execution
 * - Declarative encoding - express every constructor and operator for our model as pure data in a recursive
 *                          tree structure
 *
 * While the real Cats Effect IO implementation uses declarative encoding, it will be easier to solve this
 * task using executable encoding, that is:
 *  - Add a `private val run: () => A` parameter to the class `IO` private constructor
 *  - Have most of the methods return a `new IO(...)`
 *
 * Ask questions in the bootcamp chat if stuck on this task.
 */
object EffectsHomework1 {

  sealed trait IO[+A] {
    import IO.{Pure, Delay, Suspend, Bind}

    def map[B](f: A => B): IO[B] = flatMap(a => Pure(f(a)))
    def flatMap[B](f: A => IO[B]): IO[B] = Bind(this, a => f(a))
    def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)
    def as[B](newValue: => B): IO[B] = flatMap(_ => Pure(newValue))
    def void: IO[Unit] = flatMap(_ => Pure(()))
    def attempt: IO[Either[Throwable, A]] = Delay(() => Try(unsafeRunSync()).toEither) // TODO: eval in run loop
    def option: IO[Option[A]] = attempt.flatMap {
      case Left(_) => Pure(None)
      case Right(a) => Pure(Some(a))
    }
    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = attempt.flatMap {
      case Left(t) => f(t)
      case Right(a) => Pure(a)
    }
    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = attempt.flatMap {
      case Left(t) => Pure(recover(t))
      case Right(a) => Pure(map(a))
    }
    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = attempt.flatMap {
      case Left(t) => recover(t)
      case Right(a) => bind(a)
    }
    def unsafeRunSync(): A = {
      @tailrec
      def loop(io: IO[A]): A = io match {
        case Pure(a) => a
        case Delay(thunk) => thunk()
        case Suspend(thunk) => loop(thunk())
        case Bind(source, f) => source match {
          case Pure(a) => loop(f(a))
          case Delay(thunk) => loop(f(thunk()))
          case Suspend(thunk) => loop(thunk().flatMap(f))
          case Bind(source2, f2) => loop(source2.flatMap(f2(_).flatMap(f)))
        }
      }
      loop(this)
    }
    def unsafeToFuture(): Future[A] = Future(unsafeRunSync())
  }

  object IO {
    final private case class Pure[+A](a: A) extends IO[A]
    final private case class Delay[+A](thunk: () => A) extends IO[A]
    final private case class Suspend[+A](thunk: () => IO[A]) extends IO[A]
    final private case class Bind[E, +A](source: IO[E], f: E => IO[A]) extends IO[A]

    def apply[A](body: => A): IO[A] = delay(body)
    def suspend[A](thunk: => IO[A]): IO[A] = Suspend(() => thunk)
    def delay[A](body: => A): IO[A] = Delay(() => body)
    def pure[A](a: A): IO[A] = Pure(a)
    def fromEither[A](e: Either[Throwable, A]): IO[A] = e match {
      case Left(t) => raiseError(t)
      case Right(a) => pure(a)
    }
    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = option match {
      case None => raiseError(orElse)
      case Some(a) => pure(a)
    }
    def fromTry[A](t: Try[A]): IO[A] = t match {
      case Failure(t) => raiseError(t)
      case Success(a) => pure(a)
    }
    def none[A]: IO[Option[A]] = pure(None)
    def raiseError[A](e: Throwable): IO[A] = delay(throw e)
    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = if (!cond) raiseError(e) else unit
    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = if (cond) raiseError(e) else unit
    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (!cond) action else unit
    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) action else unit
    def unit: IO[Unit] = pure(())
  }
}
