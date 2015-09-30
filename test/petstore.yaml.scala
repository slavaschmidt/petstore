
package petstore

import de.zalando.play.controllers.PlayBodyParsing

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Test._

import org.specs2.mutable._
import play.api.test.Helpers._
import play.api.test._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import java.net.URLEncoder

import controllers._
import generatorDefinitions._

/**
* @since 30.09.2015 18:18:07
*/


  package controllers {
  import scala.Option
  import definitions.Pet
  import definitions.Error

  @RunWith(classOf[JUnitRunner])
  class PetstoreYamlSpec extends Specification {
    // TODO use specs2 scalacheck integration instead of doing this manually
    def checkResult(props: Prop) =
      Test.check(Test.Parameters.default, props).status match {
        case Failed(_, labels) => failure(labels.mkString("\n"))
        case Proved(_) | Exhausted | Passed => success
        case PropException(_, _, labels) => failure(labels.mkString("\n"))
      }

    "GET /v1/pets" should {
    
        def testInvalidInput(in: (Option[Int])) = {
          val (limit) = in
          val url = s"""/v1/pets?${limit.map { i => "limit=" + URLEncoder.encode(i.toString, "UTF-8")}.getOrElse("")}"""
          val path = route(FakeRequest(GET, url)).get
          val validation = new ValidationForPetstoreYamllistPets(limit).result
          lazy val validations = validation.left.get flatMap {
            _.messages map { m => contentAsString(path).contains(m) ?= true }
          }
          ("given an URL: [" + url + "]") |: all(
            status(path) ?= BAD_REQUEST,
            contentType(path) ?= Some("application/json"),
            validation.isLeft ?= true,
            all(validations:_*)
          )
        }
        def testValidInput(in: (Option[Int])) = {
          val (limit) = in
          val url = s"""/v1/pets?${limit.map { i => "limit=" + URLEncoder.encode(i.toString, "UTF-8")}.getOrElse("")}"""
          val path = route(FakeRequest(GET, url)).get
          ("given an URL: [" + url + "]") |: (status(path) ?= OK)
        }
         "discard invalid data" in new WithApplication {
          val genInputs =
            for {
               limit <- Gen.option(arbitrary[Int])
             } yield (limit)
          val inputs = genInputs suchThat { i => new ValidationForPetstoreYamllistPets(i).result != Right(i) }
          val props = forAll(inputs) { i => testInvalidInput(i) }
          checkResult(props)
        }
  
       
         "do something with valid data" in new WithApplication {
          val genInputs =
            for {
               limit <- Gen.option(arbitrary[Int])
             } yield (limit)
          val inputs = genInputs suchThat { i => new ValidationForPetstoreYamllistPets(i).result == Right(i) }
          val props = forAll(inputs) { i => testValidInput(i) }
          checkResult(props)
        }
  
       
  
    }

    "POST /v1/pets" should {
    
        def testInvalidInput(in: (Pet)) = {
          val (pet) = in
          val url = s"""/v1/pets"""
          val body = PlayBodyParsing.jacksonMapper("application/json").writeValueAsString(pet)
          val path = route(FakeRequest(POST, url).withBody(body)).get
          val validation = new ValidationForPetstoreYamlcreatePets(pet).result
          lazy val validations = validation.left.get flatMap {
            _.messages map { m => contentAsString(path).contains(m) ?= true }
          }
          ("given an URL: [" + url + "]"+ "and body [" + body + "]") |: all(
            status(path) ?= BAD_REQUEST,
            contentType(path) ?= Some("application/json"),
            validation.isLeft ?= true,
            all(validations:_*)
          )
        }
        def testValidInput(in: (Pet)) = {
          val (pet) = in
          val url = s"""/v1/pets"""
          val body = PlayBodyParsing.jacksonMapper("application/json").writeValueAsString(pet)
          val path = route(FakeRequest(POST, url).withBody(body)).get
          ("given an URL: [" + url + "]"+ "and body [" + body + "]") |: (status(path) ?= OK)
        }
         "discard invalid data" in new WithApplication {
          val genInputs =
            for {
               pet <- PetGenerator
             } yield (pet)
          val inputs = genInputs suchThat { i => new ValidationForPetstoreYamlcreatePets(i).result != Right(i) }
          val props = forAll(inputs) { i => testInvalidInput(i) }
          checkResult(props)
        }
  
       
         "do something with valid data" in new WithApplication {
          val genInputs =
            for {
               pet <- PetGenerator
             } yield (pet)
          val inputs = genInputs suchThat { i => new ValidationForPetstoreYamlcreatePets(i).result == Right(i) }
          val props = forAll(inputs) { i => testValidInput(i) }
          checkResult(props)
        }
  
       
  
    }

    "GET /v1/pets/{petId}" should {
    
        def testInvalidInput(in: (Int)) = {
          val (petId) = in
          val url = s"""/v1/pets/${petId}"""
          val path = route(FakeRequest(GET, url)).get
          val validation = new ValidationForPetstoreYamlshowPetById(petId).result
          lazy val validations = validation.left.get flatMap {
            _.messages map { m => contentAsString(path).contains(m) ?= true }
          }
          ("given an URL: [" + url + "]") |: all(
            status(path) ?= BAD_REQUEST,
            contentType(path) ?= Some("application/json"),
            validation.isLeft ?= true,
            all(validations:_*)
          )
        }
        def testValidInput(in: (Int)) = {
          val (petId) = in
          val url = s"""/v1/pets/${petId}"""
          val path = route(FakeRequest(GET, url)).get
          ("given an URL: [" + url + "]") |: (status(path) ?= OK)
        }
         "discard invalid data" in new WithApplication {
          val genInputs =
            for {
               petId <- arbitrary[Int]
             } yield (petId)
          val inputs = genInputs suchThat { i => new ValidationForPetstoreYamlshowPetById(i).result != Right(i) }
          val props = forAll(inputs) { i => testInvalidInput(i) }
          checkResult(props)
        }
  
       
         "do something with valid data" in new WithApplication {
          val genInputs =
            for {
               petId <- arbitrary[Int]
             } yield (petId)
          val inputs = genInputs suchThat { i => new ValidationForPetstoreYamlshowPetById(i).result == Right(i) }
          val props = forAll(inputs) { i => testValidInput(i) }
          checkResult(props)
        }
  
       
  
    }

  }
  }