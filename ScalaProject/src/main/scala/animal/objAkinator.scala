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
  
  
  
  
  
  
  
  
  
  
  /* Test pour la question 5 */
  
  /*def prefixe(s : String, n : Int) : String = {
    if(n > 1) prefixe(s, n-1)+s(n-1).toString() else s(0).toString
  }
  
  def suffixe(s : String, n : Int) : String = {
    if(n < s.length-1) s(n).toString()+suffixe(s, n+1) else s(s.length-1).toString
  }
  
  def fichierToABanimal(nomf : String) : ABanimal = {
    val l = scala.io.Source.fromFile(nomf).getLines.toList
    
    def auxFichierToAnimal(list : List[String], filsDroit : Boolean) : ABanimal = list match{
      case t::Nil => Animal(t)
      case t::t1::t2::q if((prefixe(t, 2) == "q:") && ((l.indexOf(t) > l.length/2) && (filsDroit)) || ((l.indexOf(t) < l.length/2) && (!filsDroit))) => Question(suffixe(t, 2), auxFichierToAnimal(t1::t2::q, false), auxFichierToAnimal(q, true))
      case t::q if(((l.indexOf(t) > l.length/2) && (filsDroit)) || ((l.indexOf(t) < l.length/2) && (!filsDroit))) => println(t + " : " + (((l.indexOf(t) > l.length/2) && (filsDroit)) || ((l.indexOf(t) < l.length/2) && (!filsDroit)))); Animal(t);
      case t::q => auxFichierToAnimal(q, filsDroit);
    }
    
    auxFichierToAnimal(l, false)
  }*/
  
  
  /*
    if((prefixe(t1, 2) != "q:") && (prefixe(t2, 2) == "q:")) Question(suffixe(t, 2), Animal(t1), auxFichierToAnimal(t1::t2::q, true))
       else if((prefixe(t1, 2) == "q:") && (prefixe(t2, 2) != "q:")) Question(suffixe(t, 2), auxFichierToAnimal(t1::t2::q, true), Animal(t2))
       else if((prefixe(t1, 2) != "q:") && (prefixe(t2, 2) != "q:")) Question(suffixe(t, 2), Animal(t1), Animal(t2))
       else Question(suffixe(t, 2), auxFichierToAnimal(t1::t2::q, true), auxFichierToAnimal(q, true))
   */
  
  
  
  
  def main(args: Array[String]){
    //jeuSimple(a, Source.stdin.getLines)
    //println(jeuLog(a, Source.stdin.getLines))
    println(jeuApprentissage(a1, Source.stdin.getLines))
    //println(fichierToABanimal("testQ5.txt"))
  }
  
  
  
}