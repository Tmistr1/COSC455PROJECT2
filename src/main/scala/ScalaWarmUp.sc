//Scala warmup question  1

def prime(n : Int) : Boolean = {
  if (n <= 1)
    false
  else if (n ==2)
    true
  else
    !(2 until (n-1)).exists(x => x == 0)
}
prime (5)
prime (6)


//Scala Warmup Question 2

def twinprimes(prime1: Int, prime2: Int): Boolean = {
  prime1 - prime2 match {
    case 2 => prime (prime1) && prime(prime2)
    case -2 => prime(prime1) && prime(prime2)
    case _ => false
  }
}
twinprimes (41, 43)
twinprimes (43, 41)
twinprimes (43, 47)
twinprimes (47, 43)
twinprimes (6,8)
twinprimes (8,6)

//Scala Warmup Question 3

def twinprimeslist(n:Int): List[Int] = {
  twinprimeslisthelper(n).reverse
}
def twinprimeslisthelper(n:Int) :List[Int] = {
  n <= 3 match {
    case true => Nil
    case false => {
      (twinprimes(n, n - 2)) match {
        case true => n :: n - 2 :: twinprimeslisthelper(n - 2).distinct
        case false => twinprimeslisthelper(n - 1).distinct
      }
    }
  }
}

//Scala Warmup Question 4

def goldbach(n: Int): Unit =
n match{
  case 'i' if n <= 2 => println("Greater than 2")
  case 'i' if n % 2 == 1 => println("Must be an even number")
  case 'i' if n % 2 == 2 == 0 =>
    var j = 1
    goldbachhelper(n, j)
}

def goldbachhelper(i : Int, j : Int): Unit = {
  var num1 = i
  var num2 = j
  j match{
    case 'j' if prime(i - j) && prime(j) => println(j + " + " + ( i - j) + " = " + i)
    case _ => goldbachhelper(num1, num2+1)
  }
}
goldbach (2)
goldbach (4)
goldbach (3) //Has to be even
goldbach (28) // 25+3