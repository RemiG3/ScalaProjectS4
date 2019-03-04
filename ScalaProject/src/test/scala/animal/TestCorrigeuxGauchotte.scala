package animal

import org.scalatest._
import scala.io._
import java.io._

class TestCorrigeuxGauchotte extends FunSuite{
  
  import objAkinator._
  
  val test = Question("Est-ce qu'il a des ailes ?",Question("Est-ce qu'il a des plumes ?",Question("Est-ce qu'il a un goitre ?",Animal("Pélican"),Animal("Pigeon")),Question("Est-ce qu'il a des poils ?",Animal("Chauve-souris"),Animal("Ptérodactyle"))),Question("Est-ce qu'il ronronne ?",Animal("Chat"),Question("Est-ce qu'il a un crinière ?",Animal("Cheval"),Animal("Chien"))))
  
  test("JeuSimple perdu"){
    assert(jeuSimple(a, Iterator("n","n","n")) === false)
  }
  
  test("JeuSimple gagné"){
    assert(jeuSimple(a, Iterator("n","n","o")) === true)
  }
  
  test("JeuLog"){
    assert(jeuLog(a, Iterator("n","n","o")) === List("n","n","o"))
  }
  
  test("jeuApprentissage"){
    
    // Test avec ajout d'un animal :
    assert(jeuApprentissage(a, Iterator("n","n","n","Cheval","Est-ce qu'il a un crinière ?", "o")) === test)
    
    // Test sans ajout d'animal :
    assert(jeuApprentissage(a, Iterator("n","n","o")) === a)
  }
  

  test("jeuSimpleJNSP"){
    assert(jeuSimpleJNSP(a,Iterator("x","x","x","n","n","n","n","o","o"),Nil))  
  }
  

  test("fichierToABanimal"){
    
    // Dans les cas normaux
    assert(fichierToABanimal("test.txt") == a)
    
    // Dans le cas où le fichier n'existe pas
    assertThrows[FileNotFoundException] {
      fichierToABanimal("test_inexistant.txt")
    }
    
    // Dans le cas où l'arbre est incomplet
    assertThrows[Exception] {
      assert(fichierToABanimal("test_incomplet.txt") == a)
    }
  }
  
  test("fichierToABanimal et ABanimalToFichier"){
    
  }
}