package examples


object WrappingFunctions {
  object RegularFunction {
    trait Caller {
      def use(str:String) = call.apply(str)
      def call: Function1[String, String]
    }

    class MyCaller extends Caller {
      def call: Function1[String, String] =
        (str: String) => { 
          str + "yayayaya" 
        }
    }

    class MyCallerImpl extends MyCaller {
      override def call: Function[String, String] = 
        super.call.compose((input: String) => {
          println("you are using: "+input)
          input
        })
      //  (str:String) => { println("you are using: "+str); super.call.apply(str) }
    }
  }

  object PartialFunctions {
    trait Caller {
        def use(str:String) = call.apply(str)
        def call: PartialFunction[String,String]
      } 

    class MyCaller extends Caller {
        def call: PartialFunction[String, String] = {
          case "one" => "1"
          case "two" => "2"
        }
      } 

    class MyCallerImpl extends MyCaller {
      override def call: PartialFunction[String, String] =
        PartialFunction.fromFunction((str: String) => {
          println("You are using: " + str)
          str
        }).andThen(super.call)

        // super.call.compose({
        //   PartialFunction.fromFunction((str: String) => {
        //     println("You are using: " + str)
        //     str
        //   })
        // })
    } 
  }


  def main(args: Array[String]): Unit = {
    {
      // import RegularFunction._
      // println( new MyCallerImpl().call("blah") )
    }
    {
      import PartialFunctions._
      println( new MyCallerImpl().call("one") )
    }
  }
}
