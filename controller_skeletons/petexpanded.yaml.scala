package petexpanded.expanded

import scala.Option
import scala.collection.Seq
import petexpanded.definitions.{NewPetDef, Pet, NewPet, Error}

import scala.util.Try


object PetStore {
  private var pets = Seq(
    Pet(Some(1), "Pluto", Some("Dog")),
    Pet(Some(2), "Tom", Some("Cat")),
    Pet(Some(1), "Jerry", Some("Mouse"))
  )
  def find(tags: Option[Seq[String]], limit: Option[Int]) = {
    val lim = limit.getOrElse(pets.size)
    val filter = tags map { tag =>
      pet: Pet => tag.isEmpty || (pet.tag.isDefined && tag.contains(pet.tag.get))
    } getOrElse {
      pet: Pet => true
    }
    val result = pets.filter(filter).take(lim)
    result
  }
  def add(pet: NewPetDef) = {
    val id = pets.flatMap(_.id).max + 1
    val newPet = new Pet(Some(id), pet.name, pet.tag)
    pets = pets :+ newPet
    newPet
  }
  def get(id: Long) = {
    pets.find(_.id.contains(id)).map(Right(_)) getOrElse Left(new NoSuchElementException)
  }
}

class PetexpandedYaml extends PetexpandedYamlBase {

  // handler for GET /pets
  def findPets = findPetsAction { in : (Option[Seq[String]], Option[Int]) =>
   val (tags, limit) = in
    Right { PetStore.find(tags, limit) }
  }

  // handler for POST /pets
  def addPet = addPetAction { in : (NewPet) =>
   val (pet) = in
    Right { PetStore.add(pet) }
  }

  // handler for GET /pets/{id}
  def findPetById = findPetByIdAction { in : (Long) =>
   val (id) = in
    PetStore.get(id)
  }

  // handler for DELETE /pets/{id}
  def deletePet = deletePetAction { in : (Long) =>
   val (id) = in
    ???
  }

}

