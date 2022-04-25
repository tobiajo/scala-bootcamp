package com.evolutiongaming.bootcamp.testing

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Test.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class JsonSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  import Json._

  implicit val params = Parameters.default.withMinSuccessfulTests(1000)

  def jsonGen: Gen[Json] =
    for {
      primitive <- Gen.oneOf(
        Arbitrary.arbitrary[Unit].map(_ => Json.JNull),
        Arbitrary.arbitrary[Boolean].map(Json.JBoolean),
        Arbitrary.arbitrary[Double].map(Json.JNumber),
        Arbitrary.arbitrary[String].map(Json.JString)
      )
      array <- Gen.nonEmptyListOf(primitive)
        .map(_.toVector)
        .map(Json.JArray)
    } yield JObject(Map("primitive" -> primitive, "array" -> array))

  "parse" should "invert print" in {
    forAll(jsonGen) { json =>
      parse(print(json)).get shouldEqual json
    }
  }
}
