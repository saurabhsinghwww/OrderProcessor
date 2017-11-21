import com.tieu.order.pizza.OrderProcessor

object Testing {
  
  def main(args: Array[String]): Unit = {
    
    OrderProcessor.process(System.in, System.out)    

  }
  
}