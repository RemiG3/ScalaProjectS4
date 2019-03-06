package animal

import org.scalatest._
import scala.io._
import java.io._

class TestCorrigeuxGauchotte extends FunSuite{
  
  import objAkinator._
  
  val testTDroiteO = Question("Est-ce qu'il a des ailes ?",Question("Est-ce qu'il a des plumes ?",Question("Est-ce qu'il a un goitre ?",Animal("Pélican"),Animal("Pigeon")),Question("Est-ce qu'il a des poils ?",Animal("Chauve-souris"),Animal("Ptérodactyle"))),Question("Est-ce qu'il ronronne ?",Animal("Chat"),Question("Est-ce qu'il a un crinière ?",Animal("Cheval"),Animal("Chien"))))
  val testTGaucheO = Question("Est-ce qu'il a des ailes ?",Question("Est-ce qu'il a des plumes ?",Question("Est-ce qu'il a un goitre ?",Question("A-t-il un plumage noir ?", Animal("Frégate superbe"), Animal("Pélican")),Animal("Pigeon")),Question("Est-ce qu'il a des poils ?",Animal("Chauve-souris"),Animal("Ptérodactyle"))),Question("Est-ce qu'il ronronne ?",Animal("Chat"),Animal("Chien")))
  val testMilieuO = Question("Est-ce qu'il a des ailes ?",Question("Est-ce qu'il a des plumes ?",Question("Est-ce qu'il a un goitre ?",Animal("Pélican"),Animal("Pigeon")),Question("Est-ce qu'il a des poils ?",Animal("Chauve-souris"),Question("Est-ce un poisson ?",Animal("Exocet"),Animal("Ptérodactyle")))),Question("Est-ce qu'il ronronne ?",Animal("Chat"),Animal("Chien")))
  val testTDroiteN = Question("Est-ce qu'il a des ailes ?",Question("Est-ce qu'il a des plumes ?",Question("Est-ce qu'il a un goitre ?",Animal("Pélican"),Animal("Pigeon")),Question("Est-ce qu'il a des poils ?",Animal("Chauve-souris"),Animal("Ptérodactyle"))),Question("Est-ce qu'il ronronne ?",Animal("Chat"),Question("Est-ce un Canidae ?",Animal("Chien"),Animal("Cheval"))))
  val testTGaucheN = Question("Est-ce qu'il a des ailes ?",Question("Est-ce qu'il a des plumes ?",Question("Est-ce qu'il a un goitre ?",Question("A-t-il un plumage gris ?", Animal("Pélican"), Animal("Frégate superbe")),Animal("Pigeon")),Question("Est-ce qu'il a des poils ?",Animal("Chauve-souris"),Animal("Ptérodactyle"))),Question("Est-ce qu'il ronronne ?",Animal("Chat"),Animal("Chien")))
  val testMilieuN = Question("Est-ce qu'il a des ailes ?",Question("Est-ce qu'il a des plumes ?",Question("Est-ce qu'il a un goitre ?",Animal("Pélican"),Animal("Pigeon")),Question("Est-ce qu'il a des poils ?",Animal("Chauve-souris"),Question("Est-ce un reptile ?",Animal("Ptérodactyle"),Animal("Exocet")))),Question("Est-ce qu'il ronronne ?",Animal("Chat"),Animal("Chien")))
  
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
    
    // Test avec ajout d'un animal tout à droite de l'arbre quand on répond "oui" à l'animal proposé :
    assert(jeuApprentissage(a, Iterator("n","n","n","Cheval","Est-ce qu'il a un crinière ?", "o")) === testTDroiteO)
    
    // Test avec ajout d'un animal tout à gauche de l'arbre quand on répond "oui" à l'animal proposé :
    assert(jeuApprentissage(a, Iterator("o","o","o","n","Frégate superbe","A-t-il un plumage noir ?", "o")) === testTGaucheO)
    
    // Test avec ajout d'un animal au milieu de l'arbre quand on répond "oui" à l'animal proposé :
    assert(jeuApprentissage(a, Iterator("o","n","n","n","Exocet","Est-ce un poisson ?", "o")) === testMilieuO)
    
    // Test avec ajout d'un animal tout à droite de l'arbre quand on répond "non" à l'animal proposé :
    assert(jeuApprentissage(a, Iterator("n","n","n","Cheval","Est-ce un Canidae ?", "n")) === testTDroiteN)
    
    // Test avec ajout d'un animal tout à gauche de l'arbre quand on répond "non" à l'animal proposé :
    assert(jeuApprentissage(a, Iterator("o","o","o","n","Frégate superbe","A-t-il un plumage gris ?", "n")) === testTGaucheN)
    
    // Test avec ajout d'un animal milieu de l'arbre quand on répond "non" à l'animal proposé :
    assert(jeuApprentissage(a, Iterator("o","n","n","n","Exocet","Est-ce un reptile ?", "n")) === testMilieuN)
    
    // Test sans ajout d'animal :
    assert(jeuApprentissage(a, Iterator("n","n","o")) === a)
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
    // Test les 2 fonctions à la fois pour vérifier la récupération de l'arbre originel (test sur différents arbres crées) :
    ABanimalToFichier("test_enregistrement.txt",testTDroiteO)
    assert(fichierToABanimal("test_enregistrement.txt") == testTDroiteO)
    
    ABanimalToFichier("test_enregistrement.txt",testTDroiteN)
    assert(fichierToABanimal("test_enregistrement.txt") == testTDroiteN)
    
    ABanimalToFichier("test_enregistrement.txt",testTGaucheO)
    assert(fichierToABanimal("test_enregistrement.txt") == testTGaucheO)
    
    ABanimalToFichier("test_enregistrement.txt",testTGaucheN)
    assert(fichierToABanimal("test_enregistrement.txt") == testTGaucheN)
    
    ABanimalToFichier("test_enregistrement.txt",testMilieuO)
    assert(fichierToABanimal("test_enregistrement.txt") == testMilieuO)
    
    ABanimalToFichier("test_enregistrement.txt",testMilieuN)
    assert(fichierToABanimal("test_enregistrement.txt") == testMilieuN)
  }
  
  test("Jeu JSP"){
    assert(true);
  }
}