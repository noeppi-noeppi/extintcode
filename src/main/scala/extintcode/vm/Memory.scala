package extintcode.vm

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Memory(val buffer: ListBuffer[Long]) extends mutable.Seq[Long] {

  override def apply(idx: Int): Long = {
    if (idx < 0) {
      throw new PartialSegmentationFault(idx, "NEGATIVE MEMORY READ")
    } else if (idx >= buffer.length) {
      0
    } else {
      buffer(idx)
    }
  }
  
  def apply(idx: Long): Long = {
    if (idx < 0) {
      throw new PartialSegmentationFault(idx, "NEGATIVE MEMORY READ")
    } else if (idx > Integer.MAX_VALUE) {
      throw new PartialSegmentationFault(idx, "MEMORY READ OVERFLOW")
    }
    apply(idx.toInt)
  }

  override def update(idx: Int, elem: Long): Unit = {
    if (idx < 0) {
      throw new PartialSegmentationFault(idx, "NEGATIVE MEMORY WRITE")
    } else if (idx >= buffer.length) {
      buffer.appendAll(List.fill(idx - (buffer.length - 1))(0))
    }
    buffer(idx) = elem
  }
  
  def update(idx: Long, elem: Long): Unit = {
    if (idx < 0) {
      throw new PartialSegmentationFault(idx, "NEGATIVE MEMORY WRITE")
    } else if (idx > Integer.MAX_VALUE) {
      throw new PartialSegmentationFault(idx, "MEMORY WRITE OVERFLOW")
    }
    update(idx.toInt, elem)
  }

  override def length: Int = Int.MaxValue

  override def iterator: Iterator[Long] = new Iterator[Long] {
    var pointer = 0
    override def hasNext: Boolean = pointer < Int.MaxValue
    override def next(): Long = apply({val a = pointer; pointer += 1; a})
  }
}
