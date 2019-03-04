package animal

import org.scalatest._
import scala.io._
import java.io._

class TestCorrigeuxGauchotte extends FunSuite{
  
  import objAkinator._
  
  test("JeuSimple perdu"){
    assert(jeuSimple(a, Iterator("n","n","n")) === false)
  }
  
  test("JeuSimple gagné"){
    assert(jeuSimple(a, Iterator("n","n","o")) === true)
  }
  
  test("JeuLog"){
    assert(jeuLog(a, Iterator("n","n","o")) === List("n","n","o"))
  }
  
  /*test("jeuApprentissage"){
    assert()
  }*/
  
}