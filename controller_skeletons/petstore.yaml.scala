package petstore.controllers

import scala.Option
import petstore.definitions.Pet
import petstore.definitions.Error

class PetstoreYaml extends PetstoreYamlBase {

  // handler for GET /pets
  def listPets = listPetsAction { in : (Option[Int]) =>
   val (limit) = in
    ???
  }

  // handler for POST /pets
  def createPets = createPetsAction { in : (Pet) =>
   val (pet) = in
    ???
  }

  // handler for GET /pets/{petId}
  def showPetById = showPetByIdAction { in : (Int) =>
   val (petId) = in
    ???
  }

}