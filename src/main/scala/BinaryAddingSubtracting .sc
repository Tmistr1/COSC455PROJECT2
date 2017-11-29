//Tejas Mistry
//Project 2
//Binary Adding Subtracting

//Test Cases

val pTest1: List[Int] = List (1, 1, 1, 1, 0)
val qTest1: List[Int] = List(1, 0, 1, 1)
val test1ExectedSolution: List[Int] = List(1, 0, 1, 0, 0, 1)

val pTest2: List[Int] = List (1, 0, 0, 1, 1, 0, 1)
val qTest2: List[Int] = List(1, 0, 0, 1, 0)
val test2ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 1, 1)

val pTest3: List[Int] = List (1, 0, 0, 1, 0, 0, 1)
val qTest3: List[Int] = List(1, 1, 0, 0, 1)
val test3ExectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1, 0)

val pTest4: List[Int] = List (1, 0, 0, 0, 1, 1, 1)
val qTest4: List[Int] = List(1, 0, 1, 1, 0)
val test4ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 0, 1)

val test5ExectedSolution: List[Int] = List(1, 1, 1, 0, 1, 1)
val test6ExectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1)


// Binary addition when there are two uneven lists
def finishBinaryAdd(remainingBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  (carryBit, remainingBits.isEmpty) match {
    case (true, true) => List(true)
    case (true, false) => !remainingBits.head :: finishBinaryAdd(remainingBits.tail, remainingBits.head)
    case (false, false) => remainingBits
  }
}

// Determines what the next bit is.
def getNextCarryBit(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  (pBit && qBit) || (pBit && carryBit) || (qBit && carryBit)
}

// This function does the binary addition of two Booleans and a carry bit.
def addBits(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  carryBit == (qBit == pBit)
}

// This does binary adition of boolean list (list doesnt have to be even)
def doBinaryAddition(pBits: List[Boolean], qBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  (qBits.isEmpty, pBits.isEmpty, carryBit) match {
    case (false, true, _) => finishBinaryAdd(qBits, carryBit)
    case (true, false, _) => finishBinaryAdd(pBits, carryBit)
    case (false, false, _) => addBits(pBits.head, qBits.head, carryBit) :: doBinaryAddition(pBits.tail, qBits.tail, getNextCarryBit(pBits.head, qBits.head, carryBit))
  }
}

//Converts integer  to boolean list.
def convertIntListToBooleanList(intList: List[Int]) = {
  intList.map {
    case 1 => true
    case 0 => false
  }
}

//Convert boolean to integer list.
def convertBooleanListToIntList(booleanList: List[Boolean]) = {
  booleanList.map{
    case true => 1
    case false => 0
  }
}

def twosCompliment(binary : List[Int]) : List[Int] ={
  if(binary.head != 0) {
    twosCompliment(binary.tail)
  }
  else
    binary.updated(binary.head, 1)
  binary
}

// This is the "main" function to do binary addition and subtraction .
def binaryAddition(pList: List[Int], qList: List[Int]) = {
  val aList : List[Boolean] = convertIntListToBooleanList(pList.reverse)
  val bList : List[Boolean] = convertIntListToBooleanList(qList.reverse)
  val addList : List[Boolean] = doBinaryAddition(aList,bList, false)
  val finalList : List[Int] = convertBooleanListToIntList(addList.reverse)
  finalList
}

def binarySubtraction(pList: List[Int], qList: List[Int]) = {
  twosCompliment(pList)
  twosCompliment(qList)
  val aList : List[Boolean] = convertIntListToBooleanList(pList.reverse)
  val bList : List[Boolean] = convertIntListToBooleanList(qList.reverse)
  val addList : List[Boolean] = doBinaryAddition(aList,bList, false)
  val finalList : List[Int] = convertBooleanListToIntList(addList.reverse)
  finalList
}

// Testing binary addition.
if (binaryAddition(pTest1, qTest1).equals(test1ExectedSolution)) println("Test 1 passes!") else println("Test 1 fails.")
if (binaryAddition(pTest2, qTest2).equals(test2ExectedSolution)) println("Test 2 passes!") else println("Test 2 fails.")
if (binaryAddition(pTest3, qTest3).equals(test3ExectedSolution)) println("Test 3 passes!") else println("Test 3 fails.")
if (binaryAddition(pTest4, qTest4).equals(test4ExectedSolution)) println("Test 4 passes!") else println("Test 4 fails.")

// Testing binary subtraction.
if (binarySubtraction(pTest2, qTest2).equals(test5ExectedSolution)) println("Test 5 passes!") else println("Test 5 fails.")
if (binarySubtraction(pTest4, qTest4).equals(test6ExectedSolution)) println("Test 6 passes!") else println("Test 6 fails.")