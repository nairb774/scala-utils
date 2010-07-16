package org.no.ip.bca.scala

object Ranges {
  import scala.collection.mutable.ListBuffer
  final case class Pair(start: Long, end: Long) {
    def withMaxLength(len: Long) = Pair(start, end.min(start + len))
  }
  
  def apply(parts: Pair*) = new Ranges(parts.toList)
  def apply(point: Long) = new Ranges(point)
  def apply(start: Long, stop: Long) = new Ranges(start, stop)
  def apply(parts: List[Pair]) = new Ranges(parts)
  
  private[Ranges] def union(l: List[Pair], center: ListBuffer[Pair], r: List[Pair]): Unit = {
    if (l.isEmpty) {
      center appendAll r
    } else if (r.isEmpty) {
      center appendAll l
    } else {
      var left = l
      var leftHead = left.head
      var leftStart = leftHead.start
      var leftEnd = leftHead.end
      
      var right = r
      var rightHead = r.head
      var rightStart = rightHead.start
      var rightEnd = rightHead.end
      
      var start = leftStart min rightStart
      var end = leftEnd min rightEnd
      var endPt = end
      do {
        endPt = end
        while (!left.isEmpty && leftStart <= end) {
          end = end max leftEnd
          
          left = left.tail
          if (!left.isEmpty) {
            leftHead = left.head
            leftStart = leftHead.start
            leftEnd = leftHead.end
          }
        }
        while (!right.isEmpty && rightStart <= end) {
          end = end max rightEnd
          
          right = right.tail
          if (!right.isEmpty) {
            rightHead = right.head
            rightStart = rightHead.start
            rightEnd = rightHead.end
          }
        }
      } while (end != endPt)
      center.append(Pair(start, end))
      union(left, center, right)
    }
  }
  
  private[Ranges] def intersect(l: List[Pair], center: ListBuffer[Pair], r: List[Pair]): Unit = {
    if (!l.isEmpty && !r.isEmpty) {
      var left = l
      var leftHead = left.head
      var leftStart = leftHead.start
      var leftEnd = leftHead.end
      
      var right = r
      var rightHead = r.head
      var rightStart = rightHead.start
      var rightEnd = rightHead.end
      
      var start = leftStart max rightStart
      var startPt = start
      do {
        startPt = start
        while (!left.isEmpty && leftEnd <= start) {
          start = start max leftStart
          
          left = left.tail
          if (!left.isEmpty) {
            leftHead = left.head
            leftStart = leftHead.start
            leftEnd = leftHead.end
          }
        }
        while (!right.isEmpty && rightEnd <= start) {
          start = start max rightStart
          
          right = right.tail
          if (!right.isEmpty) {
            rightHead = right.head
            rightStart = rightHead.start
            rightEnd = rightHead.end
          }
        }
      } while (start != startPt)
      var end = leftEnd min rightEnd
      if (end > start) center.append(Pair(start, end))
      left = if (left.isEmpty) Nil else left.tail
      if (leftEnd > end) left ::= Pair(end + 1, leftEnd)
      right = if (right.isEmpty) Nil else right.tail
      if (rightEnd > end) right ::= Pair(end + 1, rightEnd)
      intersect(left, center, right)
    }
  }

  private[Ranges] def complement(l: List[Pair], center: ListBuffer[Pair], r: List[Pair]): Unit = {
    if (r.isEmpty) {
      center appendAll l
    } else if (l.isEmpty) {
      // Do nothing
    } else {
      var left = l
      val leftHead = left.head
      val leftStart = leftHead.start
      val leftEnd = leftHead.end

      var right = r
      while (!right.isEmpty && right.head.end <= leftStart) {
        right = right.tail
      }
      val (rightStart, rightEnd) = if (right.isEmpty) {
        (leftStart - 1, leftStart)
      } else {
        val rightHead = right.head
        (rightHead.start, rightHead.end)
      }
      val start = leftStart
      val end = leftEnd min rightStart

      left = if(left.isEmpty) Nil else left.tail
      if (leftEnd > rightEnd) {
        left ::= Pair(rightEnd, leftEnd)
      }
      if (end > start) {
        center.append(Pair(start, end))
      }
      complement(left, center, right)
    }
  }
}

class Ranges(private val parts: List[Ranges.Pair]) {
  import scala.collection.mutable.ListBuffer
  def this(point: Long) = this(List(Ranges.Pair(point, point + 1)))
  def this(start: Long, end: Long) = this(List(Ranges.Pair(start, end)))
  def this(parts: Ranges.Pair*) = this(parts.toList)
  
  def |(point: Long) = union(point)
  def |(start: Long, end: Long) = union(start, end)
  def |(point: Ranges.Pair) = union(point)
  def |(other: Ranges) = union(other)
  def union(point: Long): Ranges = union(new Ranges(point))
  def union(start: Long, end: Long): Ranges = union(new Ranges(start, end))
  def union(point: Ranges.Pair): Ranges = union(new Ranges(point))
  def union(other: Ranges): Ranges = {
    val out = new ListBuffer[Ranges.Pair]
    Ranges.union(parts, out, other.parts)
    Ranges(out.toList)
  }
  
  def &(point: Long) = intersect(point)
  def &(start: Long, end: Long) = intersect(start, end)
  def &(point: Ranges.Pair) = intersect(point)
  def &(other: Ranges) = intersect(other)
  def intersect(point: Long): Ranges = intersect(new Ranges(point))
  def intersect(start: Long, end: Long): Ranges = intersect(new Ranges(start, end))
  def intersect(point: Ranges.Pair): Ranges = intersect(new Ranges(point))
  def intersect(other: Ranges): Ranges = {
    val out = new ListBuffer[Ranges.Pair]
    Ranges.intersect(parts, out, other.parts)
    Ranges(out.toList)
  }

  def /(point: Long) = complement(point)
  def /(start: Long, end: Long) = complement(start, end)
  def /(point: Ranges.Pair) = complement(point)
  def /(other: Ranges) = complement(other)
  def complement(point: Long): Ranges = complement(new Ranges(point))
  def complement(start: Long, end: Long): Ranges = complement(new Ranges(start, end))
  def complement(point: Ranges.Pair): Ranges = complement(new Ranges(point))
  def complement(other: Ranges): Ranges = {
    val out = new ListBuffer[Ranges.Pair]
    Ranges.complement(parts, out, other.parts)
    Ranges(out.toList)
  }
  
  def head = parts.head
  def map[B](f: Ranges.Pair => B) = parts map f
  def foreach(f: Ranges.Pair => Unit) = parts foreach f
  def length = parts.length
  def isEmpty = parts.isEmpty
  override def hashCode = parts.hashCode
  override def equals(other: Any) = parts == other.asInstanceOf[Ranges].parts
  override def toString = parts.toString
}