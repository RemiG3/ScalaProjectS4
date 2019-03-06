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
                                val rep = it.next()
                                if(rep == "o"){
                                  jeuSimple(o, it)
                                } else if (rep == "n"){
                                  jeuSimple(n, it)
                                }else{
                                  throw new Exception("Réponse non valide")
                                }
                              } 
    case Animal(ab) => {
                        println("Pensez-vous à : " + ab)
                        val rep = it.next()
                          if(rep == "o"){
                            println("J'ai gagné !")
                            true
                          } else if(rep == "n"){
                            println("J'ai perdu !")
                            false
                          }else{
                            throw new Exception("Réponse non valide")
                          }
                        }
  }
  
  
  /* Question 3 */
  
  def jeuLog(a : ABanimal, it : Iterator[String]):List[String] = a match{
    case Question(s, o, n) => {
                               println(s)
                               val rep = it.next()  
                               if(rep == "o"){
                                 "o"::jeuLog(o, it)
                               } else if (rep == "n"){
                                 "n"::jeuLog(n, it)
                               } else {
                                 throw new Exception("Réponse non valide")
                               }
                              } 
    case Animal(ab) => {
                        println("Pensez-vous à : " + ab)
                        val rep = it.next()
                          if(rep == "o"){
                            println("J'ai gagné !")
                            "o"::Nil
                          } else if (rep == "n"){
                            println("J'ai perdu !")
                            "n"::Nil
                          } else {
                            throw new Exception("Réponse non valide")
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
                        val rep = it.next()
                        if(rep == "o"){
                          println("J'ai gagné !")
                          a
                        } else if(rep == "n"){
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
                        }else{
                          throw new Exception("Réponse non valide")
                        }
                      }
    
    case Question(s, Animal(abO), question) => {
                        
                        println(s)
                        val rep = it.next()
                        if (rep == "o"){
                          println("Pensez-vous à : " + abO)
                          val rep2 = it.next()
                          if(rep2 == "o"){
                            println("J'ai gagné !")
                            a
                          } else if(rep2 == "n"){
                            println("J'ai perdu ! Quelle est la bonne réponse ? ")
                            val res = it.next();
                            println("Quelle question permet de différencier " + res + " de " + abO +" ?")
                            val q = it.next();
                            println("Quelle est la réponse à cette question pour " + res + " ?")
                            val repQ = it.next();
                            
                            if(repQ == "o")
                              Question(s, Question(q, Animal(res), Animal(abO.toString())),question)
                            else
                              Question(s, Question(q, Animal(abO.toString()), Animal(res)), question )
                            
                          }else{
                            throw new Exception("Reponse non valide")
                          }

                        } else if(rep == "n"){
                          Question(s, Animal(abO), jeuApprentissage(question, it))
                        }else{
                          throw new Exception("Reponse non valide")
                        }
                        
                      }
    
    case Question(s, question, Animal(abN)) => {
                        
                        println(s)
                        val rep = it.next()
                        if (rep == "n"){
                          println("Pensez-vous à : " + abN)
                          val rep2 = it.next()
                          if(rep2 == "o"){
                            println("J'ai gagné !")
                            a
                          } else if(rep2 == "n"){
                            println("J'ai perdu ! Quelle est la bonne réponse ? ")
                            val res = it.next();
                            println("Quelle question permet de différencier " + res + " de " + abN +" ?")
                            val q = it.next();
                            println("Quelle est la réponse à cette question pour " + res + " ?")
                            val repQ = it.next();
                            
                            if(repQ == "o")
                              Question(s,question, Question(q, Animal(res), Animal(abN.toString())) )
                            else if(repQ == "n")
                              Question(s, question, Question(q, Animal(abN.toString()), Animal(res)))
                            else
                              throw new Exception("Réponse non valide")
                          }else{
                            throw new Exception("Réponse non valide")
                          }

                        } else if (rep == "n"){
                          Question(s, jeuApprentissage(question, it), Animal(abN))
                        } else{
                          throw new Exception("Réponse non valide")
                        }
                      }
    
    case Question(s, o, n) => {
                                println(s)
                                val rep = it.next()
                                if(rep == "o"){
                                  Question(s, jeuApprentissage(o, it), n)
                                } else if(rep == "n"){
                                  Question(s, o, jeuApprentissage(n, it))
                                } else{
                                  throw new Exception("Réponse non valide")
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
  

    def jeuSimpleJNSP(a:ABanimal, it:Iterator[String],arbresRestant : List[ABanimal]) : Boolean = a match{
    	case Question(s,o,n) => {
    		println(s)
    		val rep = it.next()
    		if(rep == "o"){
    			jeuSimpleJNSP(o,it,arbresRestant)
    		} else if(rep == "n"){
    			jeuSimpleJNSP(n,it,arbresRestant)
    		} else if(rep == "x"){
    		  val resO = jeuSimpleJNSP(o,it,n::arbresRestant)
          if(!resO)
            jeuSimpleJNSP(n,it,arbresRestant)
          else
            true	
    		}else{
    		  throw new Exception("Réponse non valide")
    		}
    	}
    	case Animal(ab) => {
    		println("Pensez-vous à : " + ab)
    		val rep = it.next()
    		if(rep == "o"){
    			println("J’ai gagné !")
    			true
    		}else if(rep == "n"){
    			if(arbresRestant == Nil){
    				println("J’ai perdu !")
    				false
    			}else{
    				false
    		}
       }else{
         throw new Exception("Réponse non valide")
       }
    	}
    }

  

  
  /* Question 8 */
  
  def interfaceTextuelle() : Unit = {
    
    val ab = fichierToABanimal("default1.txt")
    
    println("Choisissez parmi les modes suivants :")
    println("- Tapez 1 pour le mode Simple.")
    println("- Tapez 2 pour le mode Apprentissage.")
    println("   Si Akinator ne connait pas encore votre animal, vous pouvez l'ajouter afin qu'il le retrouve la prochaine fois.")
    println("- Tapez 3 pour le mode SimpleJeNeSaisPas.")
    println("   Si vous ne connaissez pas la réponse à une question, vous pouvez tapez x pour passer la question")
    println("- Tapez 4 pour le mode Log.")
    println("   Ce mode fait apparaitre la liste de vos réponses à la fin de la partie.")
    
    val rep = Source.stdin.getLines.next
    
    rep match{
      case "1"  => jeuSimple(ab,Source.stdin.getLines)
      case "2"  => {
        val newAb = jeuApprentissage(ab, Source.stdin.getLines)
        println(newAb)
        ABanimalToFichier("default.txt", newAb)
      }
      case "3"  => jeuSimpleJNSP(ab,Source.stdin.getLines,Nil)
      case "4"  => println(jeuLog(ab,Source.stdin.getLines))
      case _ => jeuSimple(ab,Source.stdin.getLines)
    }
    
    println("Voulez-vous rejouer ? ")
    
    if(Source.stdin.getLines.next() == "o")
      interfaceTextuelle()      
  }
  
  
  def main(args: Array[String]){
    //jeuSimple(a, Source.stdin.getLines)
    //println(jeuLog(a, Source.stdin.getLines))
    //println(jeuApprentissage(a1, Source.stdin.getLines))
    //ABanimalToFichier("test.txt", a)
    //println(fichierToABanimal("default.txt"))
    println("Bienvenue sur Akinator des animaux !")
    println()
    println("Pour répondre aux différentes questions :")
    println("   - Tapez o pour répondre oui")
    println("   - Tapez n pour répondre non")
    println()
    
    try{
      interfaceTextuelle()
    }catch{
      case e : Exception => println("Exception : " + e);
    }
    //jeuSimpleJNSP(a, Source.stdin.getLines,Nil)


  }
  
  
  
}