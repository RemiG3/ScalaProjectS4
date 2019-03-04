package animal

import scala.io._
import java.io._

object objAkinator {
  
  trait ABanimal
  case class Animal(nom:String) extends ABanimal
  case class Question(q:String, oui:ABanimal, non:ABanimal) extends ABanimal
 
  
  val a = Question("Est-ce qu'il a des ailes ?",
            Question("Est-ce qu'il a des plumes ?",
                Question("Est-ce qu'il a un goitre ?",
                  Animal("Pélican"),Animal("Pigeon")),
                Question("Est-ce qu'il a des poils ?",
                  Animal("Chauve-souris"),Animal("Ptérodactyle"))),
            Question("Est-ce qu'il ronronne ?",
              Animal("Chat"),Animal("Chien")))
              
  val a1 = Question("Est-ce qu'il a des ailes ?",
            Question("Est-ce qu'il a des plumes ?",
                Animal("Pigeon"),
                Question("Est-ce qu'il a des poils ?",
                  Animal("Chauve-souris"),Animal("Ptérodactyle"))),
            Animal("Chien"))
  

            
  /* Question 1/2 */
  
  def jeuSimple(a : ABanimal, it : Iterator[String]) : Boolean = a match{
    case Question(s, o, n) => {
                              println(s)
                                if(it.next() == "o"){
                                  jeuSimple(o, it)
                                } else{
                                  jeuSimple(n, it)
                                }
                              } 
    case Animal(ab) => {
                        println("Pensez-vous à : " + ab)
                          if(it.next() == "o"){
                            println("J'ai gagné !")
                            true
                          } else{
                            println("J'ai perdu !")
                            false
                          }
                        }
  }
  
  
  /* Question 3 */
  
  def jeuLog(a : ABanimal, it : Iterator[String]):List[String] = a match{
    case Question(s, o, n) => {
                              println(s)
                                if(it.next() == "o"){
                                  "o"::jeuLog(o, it)
                                } else{
                                  "n"::jeuLog(n, it)
                                }
                              } 
    case Animal(ab) => {
                        println("Pensez-vous à : " + ab)
                          if(it.next() == "o"){
                            println("J'ai gagné !")
                            "o"::Nil
                          } else{
                            println("J'ai perdu !")
                            "n"::Nil
                          }
                        }
  }
  
  
  /* Question 4 */
  
  def jeuApprentissage(a : ABanimal, it : Iterator[String]) : ABanimal = a match{
    case Animal(ab) => a
    case Question(s, Animal(abO), Animal(abN)) => {
                        
                        println(s)
                        val ab = if (it.next() == "o") abO else abN;
      
                        println("Pensez-vous à : " + ab)
                        
                        if(it.next() == "o"){
                          println("J'ai gagné !")
                          a
                        } else{
                          println("J'ai perdu ! Quelle est la bonne réponse ? ")
                          val res = it.next();
                          println("Quelle question permet de différencier " + res + " de " + ab +" ?")
                          val q = it.next();
                          println("Quelle est la réponse à cette question pour " + res + " ?")
                          val repQ = it.next();
                          
                          if(ab.toString() == abN.toString()){
                            if(repQ == "o")
                              Question(s, Animal(abO), Question(q, Animal(res), Animal(ab.toString())))
                            else
                              Question(s, Animal(abO), Question(q, Animal(ab.toString()), Animal(res)))
                            
                          } else{
                            if(repQ == "o")
                              Question(s, Question(q, Animal(res), Animal(ab.toString())), Animal(abN))
                            else
                              Question(s, Question(q, Animal(ab.toString()), Animal(res)), Animal(abN))
                            
                          }
                        }
                      }
    
    case Question(s, Animal(abO), question) => {
                        
                        println(s)
                        if (it.next() == "o"){
                          println("Pensez-vous à : " + abO)
                        
                          if(it.next() == "o"){
                            println("J'ai gagné !")
                            a
                          } else{
                            println("J'ai perdu ! Quelle est la bonne réponse ? ")
                            val res = it.next();
                            println("Quelle question permet de différencier " + res + " de " + abO +" ?")
                            val q = it.next();
                            println("Quelle est la réponse à cette question pour " + res + " ?")
                            val repQ = it.next();
                            
                            if(repQ == "o")
                              Question(s, Animal(abO), Question(q, Animal(res), Animal(abO.toString())))
                            else
                              Question(s, Animal(abO), Question(q, Animal(abO.toString()), Animal(res)))
                            
                          }

                        } else{
                          Question(s, Animal(abO), jeuApprentissage(question, it))
                        }
                        
                      }
    
    case Question(s, question, Animal(abN)) => {
                        
                        println(s)
                        if (it.next() == "o"){
                          println("Pensez-vous à : " + abN)
                        
                          if(it.next() == "o"){
                            println("J'ai gagné !")
                            a
                          } else{
                            println("J'ai perdu ! Quelle est la bonne réponse ? ")
                            val res = it.next();
                            println("Quelle question permet de différencier " + res + " de " + abN +" ?")
                            val q = it.next();
                            println("Quelle est la réponse à cette question pour " + res + " ?")
                            val repQ = it.next();
                            
                            if(repQ == "o")
                              Question(s, Question(q, Animal(res), Animal(abN.toString())), Animal(abN))
                            else
                              Question(s, Question(q, Animal(abN.toString()), Animal(res)), Animal(abN))
                            
                          }

                        } else{
                          Question(s, jeuApprentissage(question, it), Animal(abN))
                        }
                        
                      }
    
    case Question(s, o, n) => {
                                println(s)
                                if(it.next() == "o"){
                                  Question(s, jeuApprentissage(o, it), n)
                                } else{
                                  Question(s, o, jeuApprentissage(n, it))
                                }
                              }
  }
  
  
  /* Question 5 */
  
 def prefixe(s : String, n : Int) : String = {
    if(n > 1) prefixe(s, n-1)+s(n-1).toString() else s(0).toString
  }
  
  def suffixe(s : String, n : Int) : String = {
    if(n < s.length-1) s(n).toString()+suffixe(s, n+1) else s(s.length-1).toString
  }
  
  def brancheDroite(l : List[String], nQ : Int, nA : Int) : List[String] = l match{
    case t::q if(nQ+1 == nA) => l
    case t::q if(prefixe(t, 2) == "q:") => brancheDroite(q, nQ+1, nA)
    case t::q => brancheDroite(q, nQ, nA+1)
  }
  
  def fichierToABanimal(nomf : String) : ABanimal = {
    def auxFichierToAnimal(list : List[String], filsDroit : Boolean) : ABanimal = list match{
      case t::q if(prefixe(t, 2) == "q:") => Question(suffixe(t, 2), auxFichierToAnimal(q, false), auxFichierToAnimal(brancheDroite(q, 0, 0), true))
      case t::q => Animal(t)
    }
    
    val l = scala.io.Source.fromFile(nomf).getLines.toList
    auxFichierToAnimal(l, false)
  }
  
  
  /* Question 6 */
  
  def ABanimalToFichier(nomf : String, a : ABanimal) : Unit = {
    
    def auxABanimalToFichier(file : FileWriter, ab : ABanimal) : Unit = ab match{
      case Animal(an) => file.write(an.toString() + "\r\n")
      case Question(q, bO, bN) => file.write("q:" + q.toString() + "\r\n"); auxABanimalToFichier(file, bO); auxABanimalToFichier(file, bN)
    }
    
    val writer = new FileWriter(new File(nomf));
    auxABanimalToFichier(writer, a)
    writer.close()
  }
  
  
  /* Question 7 */
  
    def jeuSimpleJNSP(a:ABanimal, it:Iterator[String]) : Boolean = a match{
    	val arbreRestants = Nil
    	case Question(s,o,n) => {
    		println(s)
    		if(it.next() == "o"){
    			jeuSimpleJNSP(o,it)
    		} else if(it.next() == "n"){
    			jeuSimpleJNSP(n,it)
    		} else{
    			val arbreRestants = n::Nil
    if(!jeuSimple(o,it)){
    jeuSimple(n,it)
    else
    true	
    		}
    	case Animal(ab) => {
    		println("Pensez-vous à : " + ab)
    		if(it.next() == "o"){
    			println("J’ai gagné !")
    			true
    		}else{
    			if(arbreRestants == Nil){
    				println("J’ai perdu !")
    				false
    			}else{
    				false
    		}
    }
  }

  
  /* Question 8 */
  
  def interfaceTextuelle() : Unit = {
    println("Vous jouez en mode apprentissage.")
    val ab = fichierToABanimal("default.txt")
    val newAb = jeuApprentissage(ab, Source.stdin.getLines)
    ABanimalToFichier("default.txt", newAb)
    
    println("Voulez-vous rejouer ? ")
    
    if(Source.stdin.getLines.next() == "o")
      interfaceTextuelle()
      
  }
  
  
  def main(args: Array[String]){
    //jeuSimple(a, Source.stdin.getLines)
    //println(jeuLog(a, Source.stdin.getLines))
    //println(jeuApprentissage(a1, Source.stdin.getLines))
    //ABanimalToFichier("default.txt", a)
    //println(fichierToABanimal("default.txt"))
    println("Bienvenue sur Akinator des animaux !")
    interfaceTextuelle()
  }
  
  
  
}