// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.daml.lf
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import scalaz.\/

class DarSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  behavior of Dar.getClass.getSimpleName

  it should "implement Functor" in forAll(darGen[Int]) { dar =>
    import scalaz.Functor

    val sut: Functor[Dar] = Dar.darFunctor

    def f(a: Int): Int = a + 100

    sut.map(dar)(f) shouldBe Dar(f(dar.main), dar.dependencies.map(f))
  }

  it should "implement Traverse" in forAll(darGen[Int]) { dar =>
    import scalaz.Traverse

    val sut: Traverse[Dar] = Dar.darTraverse

    def f(a: Int): Int = a + 10

    def g(a: Int): String \/ Int = \/.right(f(a))

    def h(a: Int): String \/ Int = \/.left(s"error-$a")

    val actual1: String \/ Dar[Int] = sut.traverseU(dar)(g)
    val expected1: String \/ Dar[Int] =
      \/.right(Dar(main = f(dar.main), dependencies = dar.dependencies.map(f)))
    actual1 shouldBe expected1

    val actual2: String \/ Dar[Int] = sut.traverseU(dar)(h)
    val expected2: String \/ Dar[Int] = \/.left(s"error-${dar.main}") // first error
    actual2 shouldBe expected2
  }

  private def darGen[A: Arbitrary]: Gen[Dar[A]] =
    for {
      main <- Arbitrary.arbitrary[A]
      dependencies <- Arbitrary.arbitrary[List[A]]
    } yield Dar[A](main, dependencies)
}
