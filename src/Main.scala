

object Main {
  val x = 10
  println(x)
//csak nem statikus típusra lehet match-elni???
 case class Jarmu()

case class Kocsi(hossz:Int, sebesseg: Int) extends Jarmu

case class Troli(hossz: Int, sebesseg: Int, utasok: Int) extends Jarmu
val k:Jarmu = Kocsi(1,1)
val t = Troli(8,5,9)

println(k match {
  case Jarmu() => ""
  case Kocsi(_,10) => "10 megy a kocsi"
  case Kocsi(1,_) => "1 meteres auto"
  case _ => "Nem nyert"
})
def add(a:Int, b:Int) = {
  a+b
}
val result = add(8,2)



def szorzas(a:Int, b:Int) = {
  a*b
}
def osszeadas(a: Int, b: Int) = {
  a+b
}
def kivonas(a:Int, b:Int) = {
  a-b
}
def osztas(a:Int, b:Int):Int = {
  a/b
}
def calc(a:Int, b:Int, operation: ((Int, Int) => Int)) = {
  operation(a, b)
}
def getFunction(sign: Char): (Int, Int) => Int =
  sign match {
    case '*' => szorzas
    case '/' => osztas
    case '-' => kivonas
    case '+' => osszeadas
    case _ => throw new Exception("Nincs ilyen műveleti jel") //??? Mi lehet itt default ág???
  }

  println(calc(5, 8, getFunction('*')))
  println(calc(2, 7, getFunction('+')))
  println(calc(80, 5, getFunction('/')))
  println(calc(10, 5, getFunction('-')))

  def append(s1:String, s2:String) = {
    s1+s2
  }
  def length(s1:String, s2:String): String = {
    if(s1.length > 20) s1
    else s2
  }

  def stringProcess(s1: String, s2:String, operation: (String, String) => String) = {
      operation(s1, s2)
    }

  println(stringProcess("alma", "korte", length))

  def getProcess(sign: Char): (String, String) => String ={
    sign match {
      case 'a' => append
      case 'l' => length
    }
  }

  println(stringProcess("alma", "medve", getProcess('a')))
  println(stringProcess("alma", "medve", getProcess('l')))
  /***********************************************************************/
  def perform(a: Int, b: Int, operation: (Int, Int) => Int) ={
    operation(a, b)
  }

  println(perform(5, 2, (a, b) => a + b))
  println(perform(4, 3, (a, b) => a * b))
  println(perform(50, 5, (a, b) => a / b))
  println(perform(40, 3, (a, b) => a - b))

  def calculator(a: Int, b: Int, sign: Char) = {
    getCommand(sign)(a, b)
  }
  def getCommand(sign: Char): (Int, Int) => Int = {
    sign match {
      case '*' => (a, b) => a * b
      case '+' => (a, b) => a + b
      case '/' => (a, b) => a / b
      case '-' => (a, b) => a - b
    }
  }
  println(calculator(3, 5, '*'))

  def anonimusCalculator(a: Int, b: Int, operation: (Int, Int) => Int) = {
    operation(a, b)
  }

  println(perform(5, 8, (a, b) => a * b))
  println(perform(10, 3, (a, b) => a + b))
  println(perform(2, 4, (a, b) => b - a))

  /*def callByValue(t: Long) = for (i <- 1 to 5) {//1x kérdezi le az értéket és ugyanazt írja ki 5x
    println(t)
    Thread.sleep(1000)
  }*/

  /*def callByName(t: => Long) = for (i <- 1 to 5) {//mind az 5 alkalmommal lekérdezi és kiírja az értéket
    println(t)
    Thread.sleep(1000)
  }*/

  //callByValue(System.nanoTime())
  //println()
  //callByName(System.nanoTime())

  /*def callByValue(r: Int) = { for(i <- 1 to 5) {
    println(r)
    Thread.sleep(1000)}
  }

  def callByName(r: => Int) = { for(i <- 1 to 5){
    println(r)
    Thread.sleep(1000)
  }}

  var r = new Random()
  println(callByValue(r.nextInt(1000)))
  println()
  println(callByName(r.nextInt(1000)))*/

  def callByName(i: () => Long) = { for(i <- 1 to 5){
    println(i)
    Thread.sleep(1000)
  }}

  println(callByName(() => System.nanoTime()))//ennek a szintaktikának van vmi spec jelentése: () => ???

  class IntSquare extends (Int => Int) {//static class-hoz hasonlít picit nem példányosítjuk csak meghívjuk a benne lévő metódust
    def apply(n: Int) = n * n
  }

  val intSquare = new IntSquare
  println(intSquare(5))

  class FunctionInt extends (Int => Int) {//csak egy fgv-t tartalmazhat az osztály ilyen esetben??? Ha igen miért használjuk így és nem csak def f()
    def apply(i: Int) = i * i
    def unapply(i: Int): Option[Int] = if(i >= 0) {
      val sqrt = Math.sqrt(i)
      if(sqrt.isValidInt) Some(sqrt.toInt)
      else None
    } else None

  }
  val func = new FunctionInt
  println(func(5))
  println(func.unapply(25))

  class stringExtension {
    //def apply(): stringExtension = new stringExtension()//?? miért ezt dobja fel alapból?
    def apply(text: String): String = text + "extension"

    def unapply(text: String): Option[String] = if(text.contains("extension")){
      Some(text.split("extension")(0))
    }
    else None
  }

  val s = new stringExtension

  println(s.apply("text"))
  println(s.unapply("textextension"))


  def perform(a: Int, b: Int)(operation: (Int, Int) =>(Int)):Int = operation(a, b)
  println(perform(2, 3)((a, b) => a + b))

  val perform2_3 = perform(2,3)_
  println(perform2_3((x, y) => x*x +y))


  class Person(var name: String, var age: Int, var weight: Double, var hight: Double, var married: Boolean, gender: String)//itt jó a var???
  {
    def this() = this("", 0)//Lehet tetszőleges számú paramétert beállító ctor? v minden paramétert be kell állítani?
    def salute = println(s"Hello, $name")
    def aging() = age += 1
    def wedding(age: Int) = {
      if(age > 20)
        married = true //ezt hogy lehet megoldani új példány??? vagy ilyenre nem használjuk a scala-t? val-al
    }
  }

  class Man(val gender: String) extends Person {
    def this() = this("masculan")
    def everything() = {
      salute
      println("age: " + age + "weight: " + weight + "hight: " + hight + "isMarried:" + married + "Gender: " + gender)
    }
  }
  val man = new Man("Tibi")
  man.everything()



}