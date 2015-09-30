
package petexpanded

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


  package expanded {
  import scala.Option
  import definitions.Pet
  import definitions.NewPet
  import definitions.Error

  @RunWith(classOf[JUnitRunner])
  class PetexpandedYamlSpec extends Specification {
    // TODO use specs2 scalacheck integration instead of doing this manually
    def checkResult(props: Prop) =
      Test.check(Test.Parameters.default, props).status match {
        case Failed(_, labels) => failure(labels.mkString("\n"))
        case Proved(_) | Exhausted | Passed => success
        case PropException(_, _, labels) => failure(labels.mkString("\n"))
      }

    "GET /api/pets" should {
    
        def testInvalidInput(in: (Option[Seq[String]], Option[Int])) = {
          val (tags, limit) = in
          val url = s"""/api/pets?${tags.map { i => "tags=" + URLEncoder.encode(i.toString, "UTF-8")}.getOrElse("")}&${limit.map { i => "limit=" + URLEncoder.encode(i.toString, "UTF-8")}.getOrElse("")}"""
          val path = route(FakeRequest(GET, url)).get
          val validation = new ValidationForPetexpandedYamlfindPets(tags, limit).result
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
        def testValidInput(in: (Option[Seq[String]], Option[Int])) = {
          val (tags, limit) = in
          val url = s"""/api/pets?${tags.map { i => "tags=" + URLEncoder.encode(i.toString, "UTF-8")}.getOrElse("")}&${limit.map { i => "limit=" + URLEncoder.encode(i.toString, "UTF-8")}.getOrElse("")}"""
          val path = route(FakeRequest(GET, url)).get
          ("given an URL: [" + url + "]") |: (status(path) ?= OK)
        }
         "discard invalid data" in new WithApplication {
          val genInputs =
            for {
               tags <- Gen.option(Gen.containerOf[List,String](arbitrary[String]))
    limit <- Gen.option(arbitrary[Int])
             } yield (tags, limit)
          val inputs = genInputs suchThat { i => new ValidationForPetexpandedYamlfindPets(i).result != Right(i) }
          val props = forAll(inputs) { i => testInvalidInput(i) }
          checkResult(props)
        }
  
       
         "do something with valid data" in new WithApplication {
          val genInputs =
            for {
               tags <- Gen.option(Gen.containerOf[List,String](arbitrary[String]))
    limit <- Gen.option(arbitrary[Int])
             } yield (tags, limit)
          val inputs = genInputs suchThat { i => new ValidationForPetexpandedYamlfindPets(i).result == Right(i) }
          val props = forAll(inputs) { i => testValidInput(i) }
          checkResult(props)
        }
  
       
  
    }

    "POST /api/pets" should {
    
        def testInvalidInput(in: (NewPet)) = {
          val (pet) = in
          val url = s"""/api/pets"""
          val body = PlayBodyParsing.jacksonMapper("application/json").writeValueAsString(pet)
          val path = route(FakeRequest(POST, url).withBody(body)).get
          val validation = new ValidationForPetexpandedYamladdPet(pet).result
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
        def testValidInput(in: (NewPet)) = {
          val (pet) = in
          val url = s"""/api/pets"""
          val body = PlayBodyParsing.jacksonMapper("application/json").writeValueAsString(pet)
          val path = route(FakeRequest(POST, url).withBody(body)).get
          ("given an URL: [" + url + "]"+ "and body [" + body + "]") |: (status(path) ?= OK)
        }
         "discard invalid data" in new WithApplication {
          val genInputs =
            for {
               pet <- generatorDefinitions.NewPetGenerator
             } yield (pet)
          val inputs = genInputs suchThat { i => new ValidationForPetexpandedYamladdPet(i).result != Right(i) }
          val props = forAll(inputs) { i => testInvalidInput(i) }
          checkResult(props)
        }
  
       
         "do something with valid data" in new WithApplication {
          val genInputs =
            for {
               pet <- generatorDefinitions.NewPetGenerator
             } yield (pet)
          val inputs = genInputs suchThat { i => new ValidationForPetexpandedYamladdPet(i).result == Right(i) }
          val props = forAll(inputs) { i => testValidInput(i) }
          checkResult(props)
        }
  
       
  
    }

    "GET /api/pets/{id}" should {
    
        def testInvalidInput(in: (Long)) = {
          val (id) = in
          val url = s"""/api/pets/${id}"""
          val path = route(FakeRequest(GET, url)).get
          val validation = new ValidationForPetexpandedYamlfindPetById(id).result
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
        def testValidInput(in: (Long)) = {
          val (id) = in
          val url = s"""/api/pets/${id}"""
          val path = route(FakeRequest(GET, url)).get
          ("given an URL: [" + url + "]") |: (status(path) ?= OK)
        }
         "discard invalid data" in new WithApplication {
          val genInputs =
            for {
               id <- arbitrary[Long]
             } yield (id)
          val inputs = genInputs suchThat { i => new ValidationForPetexpandedYamlfindPetById(i).result != Right(i) }
          val props = forAll(inputs) { i => testInvalidInput(i) }
          checkResult(props)
        }
  
       
         "do something with valid data" in new WithApplication {
          val genInputs =
            for {
               id <- arbitrary[Long]
             } yield (id)
          val inputs = genInputs suchThat { i => new ValidationForPetexpandedYamlfindPetById(i).result == Right(i) }
          val props = forAll(inputs) { i => testValidInput(i) }
          checkResult(props)
        }
  
       
  
    }

    "DELETE /api/pets/{id}" should {
    
        def testInvalidInput(in: (Long)) = {
          val (id) = in
          val url = s"""/api/pets/${id}"""
          val path = route(FakeRequest(DELETE, url)).get
          val validation = new ValidationForPetexpandedYamldeletePet(id).result
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
        def testValidInput(in: (Long)) = {
          val (id) = in
          val url = s"""/api/pets/${id}"""
          val path = route(FakeRequest(DELETE, url)).get
          ("given an URL: [" + url + "]") |: (status(path) ?= OK)
        }
         "discard invalid data" in new WithApplication {
          val genInputs =
            for {
               id <- arbitrary[Long]
             } yield (id)
          val inputs = genInputs suchThat { i => new ValidationForPetexpandedYamldeletePet(i).result != Right(i) }
          val props = forAll(inputs) { i => testInvalidInput(i) }
          checkResult(props)
        }
  
       
         "do something with valid data" in new WithApplication {
          val genInputs =
            for {
               id <- arbitrary[Long]
             } yield (id)
          val inputs = genInputs suchThat { i => new ValidationForPetexpandedYamldeletePet(i).result == Right(i) }
          val props = forAll(inputs) { i => testValidInput(i) }
          checkResult(props)
        }
  
       
  
    }

  }
  }