package barneshut

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.math._
import scala.collection.parallel._
import barneshut.conctrees.ConcBuffer

@RunWith(classOf[JUnitRunner])
class BarnesHutSuite extends FunSuite {

  // test cases for quad tree

import FloatOps._
  test("Empty: center of mass should be the center of the cell") {
    val quad = Empty(51f, 46.3f,  5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  test("Empty: mass should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  test("Empty: total should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  test("Leaf with 1 body") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }


  test("Fork with 3 empty quadrants and 1 leaf (nw)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Empty.insert(b) should return a Leaf with only that body") {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  test("Insert into Fork with 3 empty quadrants and 1 leaf") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    var quad = Fork(nw, ne, sw, se)


    val b1 = new Body(3f, 21f, 26f, 0f, 0f)
    quad = quad.insert(b1)

    assert(quad.ne.total == 1, s"quad size ${quad.ne.total} should be 1")
  }

  test("Insert into Leaf with one body") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quadLeaf = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val b1 = new Body(3f, 16f, 26f, 0f, 0f)
    quadLeaf.insert(b1) match {
      case quad:Fork => {
        assert(quad.nw.total == 1, s"quad nw total ${quad.nw.total} should be 1")
        assert(quad.nw.size.toFloat == 2.5f, s"quad nw size ${quad.nw.size} should be 2.5")
        assert(quad.nw.centerX == 16.25f, s"quad nw centerX ${quad.nw.centerX} should be 16.25")
        assert(quad.nw.centerY == 26.25f, s"quad nw centerY ${quad.nw.centerY} should be 26.25")

        assert(quad.ne.total == 1, s"quad ne total ${quad.ne.total} should be 1")
        assert(quad.ne.size.toFloat == 2.5f, s"quad ne size ${quad.ne.size} should be 2.5")
        assert(quad.ne.centerX == 18.75f, s"quad ne centerX ${quad.ne.centerX} should be 18.75f")
        assert(quad.ne.centerY == 26.25f, s"quad ne centerY ${quad.ne.centerY} should be 26.25f")

        assert(quad.sw.total == 0, s"quad sw total ${quad.sw.total} should be 1")
        assert(quad.sw.size.toFloat == 2.5f, s"quad sw size ${quad.sw.size} should be 2.5")
        assert(quad.sw.centerX == 16.25f, s"quad sw centerX ${quad.sw.centerX} should be 16.25")
        assert(quad.sw.centerY == 28.75f, s"quad sw centerY ${quad.sw.centerY} should be 28.75f")

        assert(quad.se.total == 0, s"quad se total ${quad.se.total} should be 1")
        assert(quad.se.size.toFloat == 2.5f, s"quad se size ${quad.se.size} should be 2.5")
        assert(quad.se.centerX == 18.75f, s"quad se centerX ${quad.se.centerX} should be 16.25")
        assert(quad.se.centerY == 28.75f, s"quad se centerY ${quad.se.centerY} should be 28.75f")
      }
    }
  }

  // test cases for Body

  test("Body.updated should do nothing for Empty quad trees") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    assert(body.xspeed == 0f)
    assert(body.yspeed == 0f)
  }

  test("Body.updated should take bodies in a Leaf into account") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    assert(body.xspeed ~= 12.587037f)
    assert(body.yspeed ~= 0.015557117f)
  }

  // test cases for sector matrix

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).find(_ == body).isDefined
    assert(res, s"Body not found in the right sector")

    val body2 = new Body(5, 97, 97, 0.1f, 0.1f)
    sm += body2
    val res2 = sm(7, 7).size == 1 && sm(7, 7).find(_ == body2).isDefined
    assert(res2, s"Body2 not found in the right sector")
  }

  test("SectorMatrix combine") {
    val body = new Body(5, 6, 7, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body

    val body2 = new Body(7, 7, 8, 0.1f, 0.1f)
    val sm2 = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm2 += body2
    val smCombined = sm.combine(sm2)
    assert(smCombined(0, 0).size == 2, s"Sectors weren't combined correctly")
  }

}

object FloatOps {
  private val precisionThreshold = 1e-4

  /** Floating comparison: assert(float ~= 1.7f). */
  implicit class FloatOps(val self: Float) extends AnyVal {
    def ~=(that: Float): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Long floating comparison: assert(double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Floating sequences comparison: assert(floatSeq ~= Seq(0.5f, 1.7f). */
  implicit class FloatSequenceOps(val self: Seq[Float]) extends AnyVal {
    def ~=(that: Seq[Float]): Boolean =
      self.size == that.size &&
        self.zip(that).forall { case (a, b) =>
          abs(a - b) < precisionThreshold
        }
  }
}

