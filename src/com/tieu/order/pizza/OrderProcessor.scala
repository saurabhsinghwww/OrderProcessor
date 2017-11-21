package com.tieu.order.pizza

import java.io.PrintStream
import java.io.InputStream
import scala.annotation.tailrec
import scala.collection.mutable
import scala.Ordering

/**
 * Companion object for finding out the minimum average waiting time for the list of customers.
 * The waiting time is calculated as the difference between the time a customer orders a pizza and time it is served.
 *
 * @author Saurabh Singh
 * @version 1.0
 */
object OrderProcessor {
  /**
   * @param in: An InputStream, which contains the following
   * input:
   * A line containing a single number: The number of guests
   * G,
   * Followed by G lines containing two numbers Oi and Di
   * separated by space.
   * There may be a trailing newline.
   * Oi ist the ordering time for Gi, Di is the time it takes
   * to bake Gi's pizza.
   * 0 <= G <= 100000
   * 0 <= Oi <= 1000000000
   * 1 <= Di <= 1000000000
   *
   * @param out: A PrintStream, which process writes the
   * following output to:
   * A single line containing the integer part of the average
   * waiting time if the input is valid.
   * A single line starting with the words "Syntax error" and
   * an optional description otherwise.
   * There may be a trailing newline.
   */
  def process(in: InputStream, out: PrintStream): Unit = {

    val lines = io.Source.fromInputStream(in).getLines()
    val guestsCount = lines.next.toInt

    // Validating the guest count
    if (!Contains(0 to 100000)(guestsCount)) {
      out.println("Syntax error")
      return ;
    }

    // Creating the guest list using the count and entry time and pizza cook time and then sorting based on entry time
    val guests: List[Guest] = (for (i <- 0 until guestsCount) yield {
      val parts = lines.next.split(" ")
      val entryTime = parts(0).toInt
      val pizzaCookTime = parts(1).toInt
      Guest(entryTime, pizzaCookTime)
    }).toList.sorted(Ordering[Int].on[Guest](_.entryTime))

    // Validating the guest entry time and pizza cook time
    if (!isValid(guests)) {
      out.println("Syntax error")
      return ;
    }

    // Finding out minimum average waiting time
    out.println(minAvgWaitingTime(guests))

  }

  /**
   * Case class for checking the element present within range or not.
   */
  private case class Contains(r: Range) { def apply(i: Int): Boolean = r contains i }

  /**
   * Case class for defining model of guest input.
   */
  private case class Guest(entryTime: Int, pizzaCookTime: Int)

  /**
   * Method for validating guest list using entry date and entry time.
   *
   * @param guests of type List[Guest]
   * @return Boolean
   */
  private def isValid(guests: List[Guest]): Boolean = {

    guests.find(p => {

      !Contains(0 to 1000000000)(p.entryTime) || !Contains(1 to 1000000000)(p.pizzaCookTime)

    }) match {

      case Some(_) => false

      case None    => true

    }

  }

  /**
   * Method for finding out minimum average waiting time by creating a priority queue using ordering parameter(entryTime + pizzaCookTime).reverse identified by entryTime and pizzaCookTime.
   *
   * @param guests of type List[Guest]
   * @return Int
   */
  private def minAvgWaitingTime(guests: List[Guest]): Int = {
    implicit val ordering = Ordering[Int].on[Guest](guest => guest.entryTime + guest.pizzaCookTime).reverse
    val queue = new mutable.PriorityQueue[Guest]()
    minAvgWaitingTime(0, 0, 0, guests, queue)
  }

  /**
   * A tail recursive method for finding out minimum average waiting time for all guests by using priority queue, time taken, served guests count and aggregation (previous aggregation + time - entryTime + pizzaCookTime) values.
   *
   * @param time
   * @param served
   * @param aggregation
   * @param guests
   * @param queue
   * @return Int
   */
  @tailrec
  private def minAvgWaitingTime(
    time:        Int,
    served:      Int,
    aggregation: Int,
    guests:      List[Guest],
    queue:       mutable.PriorityQueue[Guest]): Int = {
    val (waiting, others) = guests.span(guest => guest.entryTime <= time)
    queue.enqueue(waiting: _*)
    if (queue.isEmpty) {
      if (served == 0) 0
      else aggregation / served
    } else {
      val consumer = queue.dequeue()
      minAvgWaitingTime(
        time + consumer.pizzaCookTime,
        served + 1,
        aggregation + time - consumer.entryTime + consumer.pizzaCookTime,
        others,
        queue)
    }

  }
}