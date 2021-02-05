package edu.colorado.csci3155.project1


/* -- Here are all the instructions to be supported --*/
sealed trait StackMachineInstruction
case class LoadI(s: String) extends StackMachineInstruction
case class  StoreI(s: String) extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {

    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack
              a map from string to double precision numbers for the environment
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified environment that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: List[Double], env: Map[String, Double], ins: StackMachineInstruction): (List[Double], Map[String, Double]) = (ins, stack) match{
      //Load
      case (LoadI(s), x1::tail) => {
        val v  = tail
        v -> env.updated(s, x1)
      }

      //Store
      case (StoreI(s), _) => {
        if(env contains s){
          val v = env(s)::stack
          v -> env
        }
        else throw new IllegalArgumentException
      }

      //Push
      case (PushI(f), _) => {
        (f :: stack) -> env
      }

      //Pop
      case (PopI, x1 :: tail) => {
        if(stack.length> 0){
          tail -> env
        }
        else throw new IllegalArgumentException
      }

      //Add
      case (AddI, x1 :: x2 :: tail) => {
        if(stack.length > 1){
          (x1 + x2 :: tail) -> env
        }
        else throw new IllegalArgumentException
      }

      //Sub
      case (SubI, x1 :: x2 :: tail ) => {
        val v1 = x1
        val v2 = x2
        (v2 - v1 :: tail) -> env
      }

      //Mult
      case (MultI, x1 :: x2 :: tail ) => {
        if(stack.length > 1){
          (x1 * x2 :: tail) -> env
        }
        else throw new IllegalArgumentException
      }

      //Div
      case (DivI, x1 :: x2 :: tail ) => {
        if(stack.length > 1){
          val v1 = x1
          val v2 = x2
          (v2 / v1 :: tail) -> env
        }
        else throw new IllegalArgumentException
      }

      //Log
      case (LogI, head :: tail ) => {
        if (stack.length > 0 && head > 0){
          (scala.math.log(head) :: tail) -> env
        }
        else throw new IllegalArgumentException
      }

      //Exp
      case (ExpI, head :: tail ) => {
        if(stack.length > 0){
          (scala.math.exp(head) :: tail) -> env
        }
        else throw new IllegalArgumentException
      }

      //Sin
      case (SinI, head :: tail ) => {
        if(stack.length > 0){
          (scala.math.sin(head) :: tail) -> env
        }
        else throw new IllegalArgumentException
      }

      //Cos
      case (CosI, head :: tail ) => {
        if(stack.length > 0){
          (scala.math.cos(head) :: tail) -> env
        }
        else throw new IllegalArgumentException
      }
    }
    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be the final environment.

       Hint: accumulator for foldLeft must be a tuple (List[Double], Map[String,Double])
             initial value of this accumulator must be (Nil, Map.empty)
             You should use emulateSingleInstruction to update the accumulator.
             It will all fit nicely once you figure it out.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Map[String, Double] = {
      val stack = Nil:List[Double]
      instructionList.foldLeft(stack -> Map.empty[String,Double])((acc, SMI:StackMachineInstruction) => {
        emulateSingleInstruction(acc._1, acc._2, SMI)
      })._2
    }
}