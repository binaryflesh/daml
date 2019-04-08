// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.daml.lf

import scalaz.{Applicative, Functor, Traverse}

import scala.language.higherKinds

case class Dar[A](main: A, dependencies: List[A]) {
  lazy val all: List[A] = main :: dependencies
}

object Dar {
  implicit val darFunctor: Functor[Dar] = new Functor[Dar] {
    override def map[A, B](fa: Dar[A])(f: A => B): Dar[B] =
      Dar[B](main = f(fa.main), dependencies = fa.dependencies.map(f))
  }

  implicit val darApplicative: Applicative[Dar] = new Applicative[Dar] {
    override def point[A](a: => A): Dar[A] = Dar(a, List.empty)

    override def ap[A, B](fa: => Dar[A])(f: => Dar[A => B]): Dar[B] = {
      val b: B = f.main(fa.main)
      val bs: List[B] = fa.dependencies.zip(f.dependencies).map { case (a, a2b) => a2b(a) }
      Dar(main = b, dependencies = bs)
    }
  }

  implicit val darTraverse: Traverse[Dar] = new Traverse[Dar] {
    override def traverseImpl[G[_]: Applicative, A, B](fa: Dar[A])(f: A => G[B]): G[Dar[B]] = {
      val G: Applicative[G] = implicitly
      val gb: G[B] = f(fa.main)
      val z: G[List[B]] = G.point(List.empty[B])
      val gbs: G[List[B]] = fa.dependencies.map(f).foldRight(z) { (gb, acc) =>
        G.apply2(gb, acc)(_ :: _)
      }
      G.apply2(gb, gbs)((b, bs) => Dar(b, bs))
    }
  }
}
